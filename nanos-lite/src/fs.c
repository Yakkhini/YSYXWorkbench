#include <fs.h>
#include <stddef.h>

size_t ramdisk_read(void *buf, size_t offset, size_t len);
size_t ramdisk_write(const void *buf, size_t offset, size_t len);

typedef size_t (*ReadFn)(void *buf, size_t offset, size_t len);
typedef size_t (*WriteFn)(const void *buf, size_t offset, size_t len);

typedef struct {
  char *name;
  size_t size;
  size_t disk_offset;
  ReadFn read;
  WriteFn write;
  size_t open_offset; // This member must be the last one
} Finfo;

enum { FD_STDIN, FD_STDOUT, FD_STDERR, FD_FB, FD_DEV_EVENTS };

size_t invalid_read(void *buf, size_t offset, size_t len) {
  panic("should not reach here");
  return 0;
}

size_t invalid_write(const void *buf, size_t offset, size_t len) {
  panic("should not reach here");
  return 0;
}

size_t serial_write(const void *buf, size_t offset, size_t len);
size_t events_read(void *buf, size_t offset, size_t len);
size_t dispinfo_read(void *buf, size_t offset, size_t len);
size_t fb_write(const void *buf, size_t offset, size_t len);

/* This is the information about all files in disk. */
static Finfo file_table[] __attribute__((used)) = {
    [FD_STDIN] = {"stdin", 0, 0, invalid_read, invalid_write},
    [FD_STDOUT] = {"stdout", 0, 0, invalid_read, serial_write},
    [FD_STDERR] = {"stderr", 0, 0, invalid_read, serial_write},
    [FD_FB] = {"/dev/fb", 0, 0, dispinfo_read, fb_write},
    [FD_DEV_EVENTS] = {"/dev/events", 0, 0, events_read, invalid_write},
#include "files.h"
};

static int file_num = sizeof(file_table) / sizeof(file_table[0]);

int fs_open(const char *pathname, int flags, int mode) {
  for (int i = 0; i < file_num; i++) {
    if (strcmp(pathname, file_table[i].name) == 0) {
      if (i > 4) {
        file_table[i].read = ramdisk_read;
        file_table[i].write = ramdisk_write;
        file_table[i].open_offset = 0;
      }
      return i;
    }
  }
  panic("no such file: %s", pathname);
  return -1;
};

size_t fs_read(int fd, void *buf, size_t len) {
  if (fd < 0 || fd >= file_num) {
    return 0;
  }
  if (file_table[fd].read == ramdisk_read &&
      file_table[fd].open_offset + len > file_table[fd].size) {
    len = file_table[fd].size - file_table[fd].open_offset;
  }
  int target_offset = file_table[fd].disk_offset + file_table[fd].open_offset;
  size_t ret = file_table[fd].read(buf, target_offset, len);
  file_table[fd].open_offset += ret;

  return ret;
};

size_t fs_write(int fd, const void *buf, size_t len) {
  if (fd < 0 || fd >= file_num) {
    return 0;
  }
  int target_offset = file_table[fd].disk_offset + file_table[fd].open_offset;
  size_t ret = file_table[fd].write(buf, target_offset, len);
  file_table[fd].open_offset += ret;

  return ret;
};

size_t fs_lseek(int fd, size_t offset, int whence) {
  if (fd < 0 || fd >= file_num) {
    return 0;
  }
  switch (whence) {
  case SEEK_SET:
    file_table[fd].open_offset = offset;
    break;
  case SEEK_CUR:
    file_table[fd].open_offset += offset;
    break;
  case SEEK_END:
    file_table[fd].open_offset = file_table[fd].size + offset;
    break;
  default:
    return 0;
  }
  return file_table[fd].open_offset;
};

size_t fs_close(int fd) {
  if (fd < 0 || fd >= file_num) {
    return -1;
  }
  file_table[fd].open_offset = 0;
  return 0;
};

char *file_name(int fd) { return file_table[fd].name; }

void init_fs() {
  // TODO: initialize the size of /dev/fb
}
