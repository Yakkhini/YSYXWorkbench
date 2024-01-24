#include <common.h>
#include <cpu/cpu.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <sdb.h>

static char *rl_gets() {
  static char *line_read = NULL;

  if (line_read) {
    free(line_read);
    line_read = NULL;
  }

  line_read = readline("\%sriz-npc > ");

  if (line_read && *line_read) {
    add_history(line_read);
  }

  return line_read;
}

static int cmd_c(char *args) {
  cpu_exec(-1);
  return 0;
}

static int cmd_si(char *args) {
  char *arg = strtok(NULL, " ");
  if (arg == NULL) {
    cpu_exec(1);
    return 0;
  }
  int istep = atoi(arg);
  printf("Excute program by %i step\n", istep);
  cpu_exec(istep);
  return 0;
}

static int cmd_q(char *args) { return -1; }

#define NR_CMD ARRLEN(cmd_table)
static struct {
  const char *name;
  const char *description;
  int (*handler)(char *);
} cmd_table[] = {
    {"c", "Continue the execution of the program", cmd_c},
    {"q", "Exit NEMU", cmd_q},
    {"si", "Execute program by step", cmd_si},

};

void sdb_mainloop() {
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
