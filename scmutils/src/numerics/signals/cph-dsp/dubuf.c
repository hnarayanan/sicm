/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>

#include "dubuf.h"

#define INPUT_PROCESS_DONE 1
#define OUTPUT_PROCESS_DONE 2

#define FILL_SEM_NUM 0
#define DRAIN_SEM_NUM 1
#define LOCK_SEM_NUM 2

#define DB_INPUT_BLOCK_SIZE(db) ((db) -> input_block_size)
#define DB_OUTPUT_BLOCK_SIZE(db) ((db) -> output_block_size)
#define DB_SHM_ID(db) ((db) -> shm_id)
#define DB_SEM_ID(db) ((db) -> sem_id)
#define DB_SHM_ADDR(db) ((db) -> shm_addr)
#define DB_BUFFER_SIZE(db) (* ((db) -> buffer_size))
#define DB_BUFFER_COUNT(db) (* ((db) -> buffer_count))
#define DB_FILL_PTR(db) (* ((db) -> fill_ptr))
#define DB_DRAIN_PTR(db) (* ((db) -> drain_ptr))
#define DB_FLAGS(db) (* ((db) -> flags))
#define DB_BUFFER(db) ((db) -> buffer)
#define DB_CLEANUP(db) ((db) -> cleanup)

static void input_process_cleanup (double_buffer db);
static void output_process_cleanup (double_buffer db);

static void shm_initialize (double_buffer db, unsigned int buffer_size);
static void shm_attach (double_buffer db, void (*cleanup) (double_buffer db));
static void shm_detach (double_buffer db);
static void shm_remove_id (double_buffer db);

static void sem_initialize (double_buffer db);
static void sem_operate (double_buffer db, short sem_num, short sem_op);
static void sem_remove_id (double_buffer db);

static unsigned int lcm (unsigned int x, unsigned int y);
static unsigned int gcd (unsigned int x, unsigned int y);

static void fatal_perror (double_buffer db, const char * s);

void
db_initialize (double_buffer db,
	       unsigned int input_block_size,
	       unsigned int output_block_size)
{
  (DB_INPUT_BLOCK_SIZE (db)) = input_block_size;
  (DB_OUTPUT_BLOCK_SIZE (db)) = output_block_size;
  /* Initialize the DB structure so that the fatal error code will
     work properly.  */
  (DB_SHM_ID (db)) = (-1);
  (DB_SEM_ID (db)) = (-1);
  (DB_CLEANUP (db)) = 0;
  shm_initialize (db, (4 * (lcm (input_block_size, output_block_size))));
  sem_initialize (db);
}

void
db_close (double_buffer db)
{
  if ((DB_CLEANUP (db)) != 0)
    (* (DB_CLEANUP (db))) (db);
  if ((DB_SHM_ID (db)) >= 0)
    shm_remove_id (db);
  if ((DB_SEM_ID (db)) >= 0)
    sem_remove_id (db);
}

void
db_attach_input (double_buffer db)
{
  shm_attach (db, input_process_cleanup);
}

static void
input_process_cleanup (double_buffer db)
{
  sem_operate (db, LOCK_SEM_NUM, (-1));
  (DB_FLAGS (db)) |= INPUT_PROCESS_DONE;
  sem_operate (db, LOCK_SEM_NUM, 1);
  shm_detach (db);
  sem_operate (db, FILL_SEM_NUM, 1);
}

void
db_attach_output (double_buffer db)
{
  shm_attach (db, output_process_cleanup);
}

static void
output_process_cleanup (double_buffer db)
{
  sem_operate (db, LOCK_SEM_NUM, (-1));
  (DB_FLAGS (db)) |= OUTPUT_PROCESS_DONE;
  sem_operate (db, LOCK_SEM_NUM, 1);
  shm_detach (db);
  sem_operate (db, DRAIN_SEM_NUM, 1);
}

char *
db_get_input_pointer (double_buffer db, unsigned int * read_size)
{
  unsigned int block_size = (DB_INPUT_BLOCK_SIZE (db));
  unsigned int buffer_size = (DB_BUFFER_SIZE (db));
  char * buffer = (DB_BUFFER (db));
  unsigned int fill_ptr = (DB_FILL_PTR (db));
  while (1)
    {
      unsigned int drain_ptr;
      unsigned int buffer_count;
      unsigned int flags;

      sem_operate (db, LOCK_SEM_NUM, (-1));
      drain_ptr = (DB_DRAIN_PTR (db));
      buffer_count = (DB_BUFFER_COUNT (db));
      flags = (DB_FLAGS (db));
      sem_operate (db, LOCK_SEM_NUM, 1);

      if ((flags & OUTPUT_PROCESS_DONE) != 0)
	return (0);
      if ((buffer_size - buffer_count) < block_size)
	{
	  /* Not enough room in the buffer; wait until the output
	     process writes some data, then test again. */
	  sem_operate (db, DRAIN_SEM_NUM, (-1));
	  continue;
	}
      if ((((fill_ptr < drain_ptr) ? drain_ptr : buffer_size)
	   - fill_ptr)
	  < block_size)
	{
	  fprintf (stderr,
		   "fill_ptr = %d, drain_ptr = %d, buffer_size = %d\n",
		   fill_ptr, drain_ptr, buffer_size);
	  fflush (stderr);
	  db_close (db);
	  abort ();
	}
      (*read_size) = block_size;
      return (buffer + fill_ptr);
    }
}

void
db_record_input (double_buffer db, unsigned int n)
{
  unsigned int fill_ptr = ((DB_FILL_PTR (db)) + n);
  sem_operate (db, LOCK_SEM_NUM, (-1));
  (DB_FILL_PTR (db)) = ((fill_ptr == (DB_BUFFER_SIZE (db))) ? 0 : fill_ptr);
  (DB_BUFFER_COUNT (db)) += n;
  sem_operate (db, LOCK_SEM_NUM, 1);
  sem_operate (db, FILL_SEM_NUM, 1);
}

char *
db_get_output_pointer (double_buffer db, unsigned int * write_size)
{
  unsigned int buffer_size = (DB_BUFFER_SIZE (db));
  char * buffer = (DB_BUFFER (db));
  unsigned int drain_ptr = (DB_DRAIN_PTR (db));
  unsigned int block_size = (DB_OUTPUT_BLOCK_SIZE (db));
  while (1)
    {
      unsigned int fill_ptr;
      unsigned int buffer_count;
      unsigned int flags;

      sem_operate (db, LOCK_SEM_NUM, (-1));
      fill_ptr = (DB_FILL_PTR (db));
      buffer_count = (DB_BUFFER_COUNT (db));
      flags = (DB_FLAGS (db));
      sem_operate (db, LOCK_SEM_NUM, 1);

      if (buffer_count < block_size)
	{
	  if ((flags & INPUT_PROCESS_DONE) != 0)
	    {
	      if (buffer_count == 0)
		return (0);
	      (*write_size) = buffer_count;
	      return (buffer + drain_ptr);
	    }
	  /* Not enough data in buffer; wait until the input process
	     reads some more data. */
	  sem_operate (db, FILL_SEM_NUM, (-1));
	  continue;
	}
      if ((((drain_ptr < fill_ptr) ? fill_ptr : buffer_size) - drain_ptr)
	  < block_size)
	{
	  fprintf (stderr,
		   "fill_ptr = %d, drain_ptr = %d, buffer_size = %d\n",
		   fill_ptr, drain_ptr, buffer_size);
	  fflush (stderr);
	  db_close (db);
	  abort ();
	}
      (*write_size) = block_size;
      return (buffer + drain_ptr);
    }
}

void
db_record_output (double_buffer db, unsigned int n)
{
  unsigned int drain_ptr = ((DB_DRAIN_PTR (db)) + n);
  sem_operate (db, LOCK_SEM_NUM, (-1));
  (DB_DRAIN_PTR (db)) = ((drain_ptr == (DB_BUFFER_SIZE (db))) ? 0 : drain_ptr);
  (DB_BUFFER_COUNT (db)) -= n;
  sem_operate (db, LOCK_SEM_NUM, 1);
  sem_operate (db, DRAIN_SEM_NUM, 1);
}

static void
shm_initialize (double_buffer db, unsigned int buffer_size)
{
  (DB_SHM_ID (db)) =
    (shmget (IPC_PRIVATE,
	     (buffer_size + (4 * (sizeof (unsigned int)))),
	     (SHM_R | SHM_W)));
  if ((DB_SHM_ID (db)) < 0)
    fatal_perror (db, "shmget");
  shm_attach (db, 0);
  (DB_FILL_PTR (db)) = 0;
  (DB_DRAIN_PTR (db)) = 0;
  (DB_BUFFER_SIZE (db)) = buffer_size;
  (DB_BUFFER_COUNT (db)) = 0;
  (DB_FLAGS (db)) = 0;
  shm_detach (db);
}

static void
shm_attach (double_buffer db, void (*cleanup) (double_buffer db))
{
  void * shm_addr = (shmat ((DB_SHM_ID (db)), ((void *) 0), 0));
  if (shm_addr == ((void *) -1))
    fatal_perror (db, "shmat");
  (DB_SHM_ADDR (db)) = shm_addr;
  {
    unsigned int * p = shm_addr;
    (db -> fill_ptr) = (p++);
    (db -> drain_ptr) = (p++);
    (db -> buffer_size) = (p++);
    (db -> buffer_count) = (p++);
    (db -> flags) = (p++);
    (DB_BUFFER (db)) = ((char *) p);
  }
  (DB_CLEANUP (db)) = cleanup;
}

static void
shm_detach (double_buffer db)
{
  (DB_CLEANUP (db)) = 0;
  if ((shmdt (DB_SHM_ADDR (db))) == (-1))
    fatal_perror (db, "shmdt");
}

static void
shm_remove_id (double_buffer db)
{
  (void) shmctl ((DB_SHM_ID (db)), IPC_RMID, ((struct shmid_ds *) 0));
}

static void
sem_initialize (double_buffer db)
{
  (DB_SEM_ID (db)) = (semget (IPC_PRIVATE, 3, (SEM_R | SEM_A)));
  if ((DB_SEM_ID (db)) < 0)
    fatal_perror (db, "semget");
  (void) semctl ((DB_SEM_ID (db)), FILL_SEM_NUM, SETVAL, 0);
  (void) semctl ((DB_SEM_ID (db)), DRAIN_SEM_NUM, SETVAL, 0);
  (void) semctl ((DB_SEM_ID (db)), LOCK_SEM_NUM, SETVAL, 1);
}

static void
sem_operate (double_buffer db, short sem_num, short sem_op)
{
  struct sembuf sops;

  (sops . sem_num) = sem_num;
  (sops . sem_op) = sem_op;
  (sops . sem_flg) = 0;

  if ((semop ((DB_SEM_ID (db)), (&sops), 1)) == (-1))
    fatal_perror (db, "semop");
}

static void
sem_remove_id (double_buffer db)
{
  (void) semctl ((DB_SEM_ID (db)), 0, IPC_RMID, 0);
}

static unsigned int
lcm (unsigned int n, unsigned int m)
{
  return ((n == 0) ? n : (m == 0) ? m : ((n * m) / (gcd (n, m))));
}

static unsigned int
gcd (unsigned int n, unsigned int m)
{
  while (m != 0)
    {
      unsigned int k = (n % m);
      n = m;
      m = k;
    }
  return (n);
}

static void
fatal_perror (double_buffer db, const char * s)
{
  perror (s);
  fflush (stderr);
  db_close (db);
  exit (1);
}
