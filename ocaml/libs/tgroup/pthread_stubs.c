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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>

#include "pthread_helpers.h"

CAMLprim value stub_pthread_self(value unit){
  CAMLparam1(unit);
  pthread_t thread;

  caml_enter_blocking_section();
  thread = pthread_self();
  caml_leave_blocking_section();

  return Val_int(thread);
}

CAMLprim value stub_pthread_set_name(value name){
  CAMLparam1(name);
  CAMLlocal1(result);
  int rc;
  const char *c_name = String_val(name);
  
  caml_enter_blocking_section();
  rc = pthread_set_name(c_name);
  caml_enter_blocking_section();

  if (rc != 0)
    CAMLreturn(Val_none);
  
  result = caml_copy_string(c_name);
  CAMLreturn(caml_alloc_some(name));
}
