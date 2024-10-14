/*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT Group WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
#define _GNU_SOURCE

#include <pthread.h>

#define NAMELEN 16

extern int pthread_set_name(const char *name){
    pthread_t thread;

    thread = pthread_self();

    return pthread_setname_np(thread, name);
  }

extern int pthread_get_name(char* thread_name){
  pthread_t thread;
  
  thread = pthread_self();

  return pthread_getname_np(thread, thread_name, NAMELEN);
}

extern int pthread_set_schedparam(const int policy, const struct sched_param *param){
  pthread_t thread;

  thread = pthread_self();

  return pthread_setschedparam(thread, policy, param);

}