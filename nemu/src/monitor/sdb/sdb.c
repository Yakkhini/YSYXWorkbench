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
#include "debug.h"
#include "utils.h"
#include <cpu/cpu.h>
#include <isa.h>
#include <memory/vaddr.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <sdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int is_batch_mode = false;

void init_regex();
void init_wp_pool();

/* We use the `readline' library to provide more flexibility to read from stdin.
 */
static char *rl_gets() {
  static char *line_read = NULL;

  if (line_read) {
    free(line_read);
    line_read = NULL;
  }

  line_read = readline("(nemu) ");

  if (line_read && *line_read) {
    add_history(line_read);
  }

  return line_read;
}

static int cmd_c(char *args) {
  cpu_exec(-1);
  return 0;
}

static int cmd_d(char *args) {
  char *arg = strtok(NULL, " ");
  int wp_no = atoi(arg);
  free_wp(wp_no);
  return 0;
}

static int cmd_si(char *args) {
  char *arg = strtok(NULL, " ");
  if (arg == NULL) {
    printf("No step number, default to one step.\n");
    cpu_exec(1);
    return 0;
  }
  int istep = atoi(arg);
  printf("Excute program by %i step\n", istep);
  cpu_exec(istep);
  return 0;
}

static int cmd_info(char *args) {
  char *arg = strtok(NULL, " ");
  if (arg == NULL) {
    printf("Please give info subcommand.\n");
  } else if (strcmp(arg, "r") == 0) {
    isa_reg_display();
  } else if (strcmp(arg, "w") == 0) {
    read_wp();
  } else {
    printf("Incorrect info subcommand.\n");
  }

  return 0;
}

static int cmd_p(char *args) {
  bool success = true;
  word_t result = expr(args, &success);
  if (success) {
    printf("Result is: %u.\n", result);
  } else {
    printf("Parse expression failed.");
  }
  return 0;
}

static int cmd_q(char *args) {
  nemu_state.state = NEMU_QUIT;
  return -1;
}

static int cmd_w(char *args) {
  WP *wp = new_WP();
  bool success = true;
  strcpy(wp->expression, args);
  wp->result = expr(wp->expression, &success);
  if (success) {
    Log("New watchpoint in NO.%i with expression %s equal to value %u", wp->NO,
        wp->expression, wp->result);
  } else {
    Log("Expression error, delete watchpoint.");
    free_wp(wp->NO);
  }
  return 0;
}

static int cmd_x(char *args) {
  char *len = strtok(NULL, " ");
  char *pos = strtok(NULL, " ");
  int ilen = strtol(len, NULL, 10);
  vaddr_t upos = strtol(pos, NULL, 16);
  printf("Reading %i data begin at 0x%X...\n", ilen, upos);
  for (int i = 0; i < ilen; i++) {
    printf("0x%08X: 0x%08X\n", upos + i, vaddr_read(upos + i, 4));
  }
  return 0;
}

static int cmd_help(char *args);

static struct {
  const char *name;
  const char *description;
  int (*handler)(char *);
} cmd_table[] = {
    {"help", "Display information about all supported commands", cmd_help},
    {"c", "Continue the execution of the program", cmd_c},
    {"d", "Free watchpoint", cmd_d},
    {"p", "Caculate expression and output value.", cmd_p},
    {"q", "Exit NEMU", cmd_q},
    {"si", "Execute program by step", cmd_si},
    {"info", "Print reg or watch point info.", cmd_info},
    {"x", "Read memory.", cmd_x},
    {"w", "Set new watchpoint.", cmd_w},

    /* TODO: Add more commands */

};

#define NR_CMD ARRLEN(cmd_table)

static int cmd_help(char *args) {
  /* extract the first argument */
  char *arg = strtok(NULL, " ");
  int i;

  if (arg == NULL) {
    /* no argument given */
    for (i = 0; i < NR_CMD; i++) {
      printf("%s - %s\n", cmd_table[i].name, cmd_table[i].description);
    }
  } else {
    for (i = 0; i < NR_CMD; i++) {
      if (strcmp(arg, cmd_table[i].name) == 0) {
        printf("%s - %s\n", cmd_table[i].name, cmd_table[i].description);
        return 0;
      }
    }
    printf("Unknown command '%s'\n", arg);
  }
  return 0;
}

void sdb_set_batch_mode() { is_batch_mode = true; }

void sdb_mainloop() {
  if (is_batch_mode) {
    cmd_c(NULL);
    return;
  }

  for (char *str; (str = rl_gets()) != NULL;) {
    char *str_end = str + strlen(str);

    /* extract the first token as the command */
    char *cmd = strtok(str, " ");
    if (cmd == NULL) {
      continue;
    }

    /* treat the remaining string as the arguments,
     * which may need further parsing
     */
    char *args = cmd + strlen(cmd) + 1;
    if (args >= str_end) {
      args = NULL;
    }

#ifdef CONFIG_DEVICE
    extern void sdl_clear_event_queue();
    sdl_clear_event_queue();
#endif

    int i;
    for (i = 0; i < NR_CMD; i++) {
      if (strcmp(cmd, cmd_table[i].name) == 0) {
        if (cmd_table[i].handler(args) < 0) {
          return;
        }
        break;
      }
    }

    if (i == NR_CMD) {
      printf("Unknown command '%s'\n", cmd);
    }
  }
}

void init_sdb() {
  /* Compile the regular expressions. */
  init_regex();

  /* Check the robust of expr() function.*/
  expr_check();

  /* Initialize the watchpoint pool. */
  init_wp_pool();
}
