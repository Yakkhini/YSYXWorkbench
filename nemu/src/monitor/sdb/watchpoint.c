/***************************************************************************************
 * Copyright (c) 2014-2022 Zihao Yu, Nanjing University
 *
 * NEMU is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan
 *PSL v2. You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY
 *KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 *NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

#include "common.h"
#include "sdb.h"
#include <assert.h>

#define NR_WP 32

typedef struct watchpoint {
  int NO;
  struct watchpoint *next;

  /* TODO: Add more members if necessary */
  bool idle;
  char *expression;
  word_t result;
} WP;

static WP wp_pool[NR_WP] = {};
static WP *head = NULL, *free_ = NULL;

void init_wp_pool() {
  int i;
  for (i = 0; i < NR_WP; i++) {
    wp_pool[i].NO = i;
    wp_pool[i].next = (i == NR_WP - 1 ? NULL : &wp_pool[i + 1]);
    wp_pool[i].idle = true;
    wp_pool[i].expression = "0";
    wp_pool[i].result = 0;
  }

  head = NULL;
  free_ = wp_pool;
}

/* TODO: Implement the functionality of watchpoint */
WP *new_WP() {
  for (int i = 0; i < NR_WP; i++) {
    if (free_[i].idle) {
      WP *using_wp = head;
      if (using_wp == NULL) {
        using_wp = &free_[i];
        free_[i].idle = false;
        free_[i].next = NULL;
        return using_wp;
      }
      while (using_wp != NULL) {
        if (using_wp->next == NULL) {
          using_wp->next = &free_[i];
          free_[i].idle = false;
        }
      }
      free_[i].next = NULL;
      return using_wp->next;
    }
  }
  assert(0);
}
void free_wp(WP *wp) {
  WP *using_wp = head;
  if (wp == head) {
    head = wp->next;
  }
  while (using_wp != wp) {
    if (using_wp->next == wp) {
      using_wp->next = wp->next;
      break;
    }
    using_wp = using_wp->next;
  }
  wp->idle = true;
  wp->next = &free_[wp->NO + 1];
}
