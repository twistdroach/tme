
**Notice:** The below is ported directly from the original website [here](https://people.csail.mit.edu/fredette/tme/)

More updates (and more complete porting of the rest of the pages) to come!

* * *

The Machine Emulator
====================

_June 6 2010_

**Warning:** This is alpha-quality software. Don't count on it for anything. Use it at your own risk.

* * *

Contents
--------

*   [Introduction](#intro)
*   [Getting the tme software](#getting)
*   [Preparing your system](#system)
    *   [Ethernet access on BSD-based systems with BPF](#system-bpf)
    *   [Serial line access on POSIX-like systems](#system-tty)
*   [The different emulations](#emu)
    *   [Emulate a Sun 2/120 running NetBSD](#sun2-120-nbsd)
    *   [Emulate a Sun 3/150 running NetBSD](#sun3-150-nbsd)
    *   [Emulate a SPARCstation 2 running NetBSD](#sun4-75-nbsd)
    *   [Emulate a Sun Ultra 1 running NetBSD](#sun-u1-nbsd)
*   [Notes, bugs, etc.](#notes)
*   [Previous versions](#older)

* * *

Introduction
------------

The Machine Emulator, or tme, provides a general-purpose framework for computer emulation. The goal is to create a large library of modules, each emulating a specific computer chip, or bus, or board, etc. These modules offer standard interfaces that allow you to connect them together to create a whole machine emulation with a minimum of effort.

While these assembled machine emulations will likely never be as fast as a single program designed from the beginning to emulate a specific machine, the standard module interfaces should be reasonably fast. Even though the best abstract interfaces still introduce overhead and can't always express certain optimization hints, reusability will always save a lot of development time, and performance problems can be addressed by faster hardware.

* * *

Getting the tme software
------------------------

2.  Download the latest version of tme:
    
    [http://csail.mit.edu/~fredette/tme/tme-0.8.tar.gz](tme-0.8.tar.gz)
    
    tme can build without any third-party libraries or programs. However:
    
    *   If you have GTK installed, tme can use it to provide framebuffer, keyboard, and mouse support to modules that emulate those things.
    *   If you want to modify certain source files, Perl is required to regenerate a small number of sources.
    *   Perl is also required to run a small number of utility scripts, such as tme-sun-idprom. However these scripts can be run anywhere - not only on the machine running tme.
    
3.  Unpack and build tme.
    
    **Note: the emulator is normally built out of plugin modules dlopen'ed at runtime**. If you intend to install the emulation software under /usr/pkg or /usr/local, as long as your LD\_LIBRARY\_PATH includes the relevant lib/ directory, everything should just work.
    
    If you want to use the emulator without installing it, however, you should use the \--disable-shared option when running configure, to build a statically-linked emulator that will do no dlopening. **However, to run a statically-linked, uninstalled emulator, you must have the environment variable LTDL\_LIBRARY\_PATH set to the top of your build tree.**
    
    **If your system has GTK but uses pkg-config instead of a gtk-config script**, put the following gtk-config script somewhere in your PATH and make it mode 0555:
    ```
    #! /bin/sh
    
    module=\`pkg-config --list-all | egrep '^gtk\\+?-' | head -1 | sed -e 's/^\\(.\[-a-z0-9\_\\+\\.\]\*\\).\*$/\\1/'\`
    if test "x${module}" = x; then
      echo "$0: no gtk under pkg-config control" 1>&2
      exit 1
    fi
    
    args=
    for option
    do
      if test "${option}" = --version; then
        option=--modversion
      fi
      args="${args} ${option}"
    done
    exec pkg-config ${args} ${module}
    ```
    This is necessary because I make tme's configure script with an ancient gtk.m4, that predates pkg-config.
    
    Otherwise, this is a regular autoconf package, so do something like:
    ```
    % **sh ./configure**
    % **make**
    ```
    Assuming the build succeeds, if you do want to install the emulator, then do a make install. The rest of this documentation assumes that you have installed the emulator under /usr/pkg. If you don't install the emulator, you'll have to find the programs and files that are referenced in the documentation in the build tree.
    

* * *

Preparing your system
---------------------

Full emulation of certain kinds of elements, like the Ethernet and serial lines, requires certain permissions on the host running tme. If you don't intend to allow tme to access the network or serial ports on your machine, you can ignore this section.

### Ethernet access on BSD-based systems with BPF

The only Ethernet access method currently supported by tme is the Berkely Packet Filter (BPF), usually available on BSD-based systems. If you want the emulator to be visible on your local Ethernet, the emulator must be able to open a bpf(4) device for reading and writing.

Normally, /dev/bpf_N_ devices can only be opened by root, so you have one of two choices:

1.  Run the emulator as root. **This is not recommended**, since the emulator won't give away its root privileges, running as root the entire time. Bugs could cause it to do real damage to the host.
2.  Make a /dev/bpf device usable by the user that will be running the emulator. If this user is in group wheel on the host machine, the /dev/bpf devices are usually group wheel, mode 0600, so one solution is to make /dev/bpf0 mode 0660. You may want to do something different.

### Serial line access on POSIX-like systems

On POSIX-like systems, tme can connect the real serial lines and pseudoterminals on your host to emulated serial lines. This is always done with the tme/host/posix/serial module, which takes a device argument with the name of any tty(4) device you want it to use.

Usually, permissions prevent the user running the emulator from connecting to the real serial lines in the host. So, you have one of two choices:

1.  Run the emulator as root. **This is not recommended**, since the emulator won't give away its root privileges, running as root the entire time. Bugs could cause it to do real damage to the host.
2.  Make the device usable by the user that will be running the emulator. If this user is in group wheel on the host machine, the /dev/tty devices are usually group wheel, mode 0600, so one solution is to make /dev/tty00 mode 0660. You may want to do something different.

Pseudoterminals generally don't have restrictive permissions, so they may be a more suitable choice for connecting to emulated serial ports. You can use one side of a pty(4) pseudoterminal pair with the tme/host/posix/serial module, however you must specify exactly which side of which pair you want to use - the emulator won't find a free pseudoterminal at runtime.

You should then be able to use any serial communications program - like tip(1) - on the other side of the pseudoterminal pair to access the serial data. For example, I can have the emulator connect /dev/ttyr0 to the first emulated serial port on an otherwise headless Sun 2, and with the following entry added to /etc/remote I can use tip sun2 to connect to the Sun 2's serial console:

sun2:dv=/dev/ptyr0:br#9600:pa=none:dc:

* * * 

The different emulations
------------------------

These pages describe how to use tme to emulate various machines and operating systems.### Emulate a Sun 2/120 running NetBSD

tme can emulate a Sun 2/120 running NetBSD. [Read the instructions here.](sun2-120-nbsd.html)### Emulate a Sun 3/150 running NetBSD

tme can emulate a Sun 3/150 running NetBSD. [Read the instructions here.](sun3-150-nbsd.html)### Emulate a SPARCstation 2 running NetBSD

tme can emulate a SPARCstation 2 running NetBSD. [Read the instructions here.](sun4-75-nbsd.html)### Emulate a Sun Ultra 1 running NetBSD

tme can emulate a Sun Ultra 1 running NetBSD. [Read the instructions here.](sun-u1-nbsd.html)

* * *

Previous versions
-----------------

[tme-0.6.tar.gz](http://csail.mit.edu/~fredette/tme/tme-0.6.tar.gz)  
[tme-0.4.tar.gz](http://csail.mit.edu/~fredette/tme/tme-0.4.tar.gz)  
[tme-0.2.tar.gz](http://csail.mit.edu/~fredette/tme/tme-0.2.tar.gz)  
[tme-0.0.tar.gz](http://csail.mit.edu/~fredette/tme/tme-0.0.tar.gz)  

* * *

Copyright © 2003, 2005, 2007, 2010 Matt Fredette, All Rights Reserved  
_$Revision: 1.7 $_
