#include <fcntl.h>
#include <linux/fb.h> /* /linux-kernel/drivers/video/fbdev */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

struct framebuffer {
    char *filename;
    int fd;
    struct fb_fix_screeninfo fix;
    struct fb_var_screeninfo var;
} fb;


void mandelbrot_view(void *buf, uint32_t width, uint32_t height, double x, double y, double scale);
int open_framebuffer(char *filename, struct framebuffer *fb);
int close_framebuffer(struct framebuffer *fb);

int
open_framebuffer(char *filename, struct framebuffer *fb) {
    int fd = open(filename, O_RDWR);

    if (fd == -1) {
        perror("Error: open framebuffer");
        return -1;
    }

    struct fb_fix_screeninfo fix;
    struct fb_var_screeninfo var;

    if (ioctl(fd, FBIOGET_FSCREENINFO, &fix) < 0) {
        perror("Error: ioctl FBIOGET_FSCREENINFO");
        close(fd);
        return -1;
    };

    if (ioctl(fd, FBIOGET_VSCREENINFO, &var) < 0) {
        perror("Error: ioctl FBIOGET_VSCREENINFO");
        close(fd);
        return -1;
    };

    fb->filename = filename;
    fb->fd = fd;
    fb->fix = fix;
    fb->var = var;

    return 0;
}

int close_framebuffer(struct framebuffer *fb) {
    if (fb->fd > 0) {
        close(fb->fd);
    }

    return 0;
}

int
main(int argc, char **argv) {
    char *filename = argv[1];

    struct framebuffer fb;
    if (open_framebuffer(filename, &fb) < 0) {
        return -1;
    }

    uint32_t buf_len = fb.fix.smem_len;

    uint8_t *buf = mmap(
            0,
            buf_len,
            PROT_READ | PROT_WRITE,
            MAP_FILE | MAP_SHARED,
            fb.fd,
            0);

    /* so we can restore the screen later */
    void *buf_save = malloc(buf_len);
    memcpy(buf_save, buf, buf_len);

    if (buf == (unsigned char *)-1) {
        perror("mmap framebuffer");
        close(fb.fd);
        return -1;
    }

    memset(buf, 0x0, buf_len);
    // draw mandelbrot
    uint32_t width = fb.var.xres;
    uint32_t height = fb.var.yres;
    double x = 0.0;
    double y = 0.0;
    double scale = 1.0;

    mandelbrot_view(buf, width, height, x, y, scale);

    sleep(2);

    // update framebuffer
    // fb.var.activate |= FB_ACTIVATE_NOW | FB_ACTIVATE_FORCE;
    // ioctl(fb.fd, FBIOPUT_VSCREENINFO, &fb.var);

    // cleanup
    memcpy(buf, buf_save, buf_len);
    free(buf_save);
    munmap(buf, buf_len);
    close_framebuffer(&fb);

    return 0;
}
