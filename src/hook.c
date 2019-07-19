// Processes in the AWS Lambda environment don't have the permissions to set a thread's name calling prctl(PR_SET_NAME).
// Async tries to do so and crashes on launch.
// All this is to ignore calls to prctl coming from Async.

#define _GNU_SOURCE
#include <linux/prctl.h>
#include <dlfcn.h>
#include <stdio.h>

typedef int (*prctl_t)(int, unsigned long, unsigned long, unsigned long, unsigned long);

int real_prctl(int option, unsigned long arg2, unsigned long arg3, unsigned long arg4, unsigned long arg5) {
    return ((prctl_t)dlsym(RTLD_NEXT, "prctl"))(option, arg2, arg3, arg4, arg5);
}

int prctl(int option, unsigned long arg2, unsigned long arg3, unsigned long arg4, unsigned long arg5) {
    if (option == PR_SET_NAME) {
        return 0;
    }

    return real_prctl(option, arg2, arg3, arg4, arg5);
}
