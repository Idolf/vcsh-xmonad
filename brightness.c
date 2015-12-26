#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static const long int MAX_BRIGHTNESS = 3000;
static const char* PATH = "/sys/class/drm/card0-LVDS-1/intel_backlight/brightness";

int main (int argc, char **argv) {
  int fd;
  long int brightness;
  char *endptr;
  if(argc != 2 || argv[1] == NULL) {
    return EXIT_FAILURE;
  }

  if(strlen(argv[1]) == 0 || strlen(argv[1]) > 6) {
    return EXIT_FAILURE;
  }

  brightness = strtol(argv[1], &endptr, 10);

  if(strlen(endptr) != 0) {
    return EXIT_FAILURE;
  }

  if(brightness < 0 || brightness > MAX_BRIGHTNESS) {
    return EXIT_FAILURE;
  }

  setuid(0);
  setgid(0);
  if ((fd = open(PATH, O_WRONLY)) == -1) {
    return EXIT_FAILURE;
  }
  write(fd, argv[1], strlen(argv[1]));
  close(fd);
  return EXIT_SUCCESS;
}
