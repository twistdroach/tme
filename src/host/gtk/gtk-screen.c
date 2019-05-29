/* $Id: gtk-screen.c,v 1.3 2003/10/29 02:03:26 fredette Exp $ */

/* host/gtk/gtk-screen.c - GTK screen support: */

/*
 * Copyright (c) 2003 Matt Fredette
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Matt Fredette.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <tme/common.h>
_TME_RCSID("$Id: gtk-screen.c,v 1.3 2003/10/29 02:03:26 fredette Exp $");

/* we are aware of the problems with gdk_image_new_bitmap, and we cope
   with them, so we define GDK_ENABLE_BROKEN to get its prototype
   under GTK 2: */
#define GDK_ENABLE_BROKEN

/* includes: */
#include "gtk-display.h"
#include <stdlib.h>

/* macros: */

/* the GTK 1.x gtk_image_new function is the GTK 2.x
   gtk_image_new_from_image function: */
#if GTK_MAJOR_VERSION == 1
#define gtk_image_new_from_image gtk_image_new
#endif /* GTK_MAJOR_VERSION == 1 */

/* the GTK screens update thread: */
void
_tme_gtk_screen_th_update(struct tme_gtk_display *display)
{
  struct tme_gtk_screen *screen;
  struct tme_fb_connection *conn_fb_other;
  int changed;
  int rc;
  
  /* loop forever: */
  for (;;) {

    /* lock the mutex: */
    tme_mutex_lock(&display->tme_gtk_display_mutex);

    /* loop over all screens: */
    for (screen = display->tme_gtk_display_screens;
	 screen != NULL;
	 screen = screen->tme_gtk_screen_next) {

      /* skip this screen if it's unconnected: */
      if (screen->tme_gtk_screen_fb == NULL) {
	continue;
      }

      /* get the other side of this connection: */
      conn_fb_other
	= ((struct tme_fb_connection *) 
	   screen->tme_gtk_screen_fb->tme_fb_connection.tme_connection_other);

      /* if the framebuffer has an update function, call it: */
      if (conn_fb_other->tme_fb_connection_update != NULL) {
	rc = (*conn_fb_other->tme_fb_connection_update)(conn_fb_other);
	assert (rc == TME_OK);
      }

      /* translate this framebuffer's contents: */
      changed = (*screen->tme_gtk_screen_fb_xlat)
	(((struct tme_fb_connection *) 
	  screen->tme_gtk_screen_fb->tme_fb_connection.tme_connection_other),
	 screen->tme_gtk_screen_fb);

      /* if those contents changed, redraw the widget: */
      if (changed) {
	gtk_widget_queue_draw(screen->tme_gtk_screen_gtkimage);
      }
    }

    /* unlock the mutex: */
    tme_mutex_unlock(&display->tme_gtk_display_mutex);

    /* update again in .5 seconds: */
    tme_thread_sleep_yield(0, 500000);
  }
  /* NOTREACHED */
}

/* this recovers the bits-per-pixel value for a GdkImage: */
static unsigned int
_tme_gtk_gdkimage_bipp(GdkImage *image)
{
  unsigned int bipp, total_bits_halved;

  total_bits_halved = image->bpl;
  total_bits_halved = (total_bits_halved * 8) / 2;
  for (bipp = image->depth;
       (bipp * image->width) <= total_bits_halved;
       bipp <<= 1);
  return (bipp);
}

/* this recovers the scanline-pad value for a GdkImage: */
static unsigned int
_tme_gtk_gdkimage_scanline_pad(GdkImage *image)
{

  if ((image->bpl % sizeof(tme_uint32_t)) == 0) {
    return (32);
  }
  if ((image->bpl % sizeof(tme_uint16_t)) == 0) {
    return (16);
  }
  if ((image->bpl % sizeof(tme_uint8_t)) == 0) {
    return (8);
  }
}

/* this is called for a mode change: */
int
_tme_gtk_screen_mode_change(struct tme_fb_connection *conn_fb)
{
  struct tme_gtk_display *display;
  struct tme_gtk_screen *screen;
  struct tme_fb_connection *conn_fb_other;
  struct tme_fb_xlat fb_xlat_q;
  const struct tme_fb_xlat *fb_xlat_a;
  int scale;
  unsigned long fb_area, avail_area, percentage;
  gint width, height;
  GdkImage *gdkimage;
  GdkVisual *visual;
  int color_count, color_i;
  GdkColor color;

  /* recover our data structures: */
  display = conn_fb->tme_fb_connection.tme_connection_element->tme_element_private;
  conn_fb_other = (struct tme_fb_connection *) conn_fb->tme_fb_connection.tme_connection_other;

  /* lock our mutex: */
  tme_mutex_lock(&display->tme_gtk_display_mutex);

  /* find the screen that this framebuffer connection references: */
  for (screen = display->tme_gtk_display_screens;
       (screen != NULL
	&& screen->tme_gtk_screen_fb != conn_fb);
       screen = screen->tme_gtk_screen_next);
  assert (screen != NULL);

  /* if the user hasn't specified a scaling, pick one: */
  scale = screen->tme_gtk_screen_fb_scale;
  if (scale < 0) {

    /* calulate the areas, in square pixels, of the emulated
       framebuffer and the host's screen: */
    fb_area = (conn_fb_other->tme_fb_connection_width
	       * conn_fb_other->tme_fb_connection_height);
    avail_area = (gdk_screen_width()
		  * gdk_screen_height());

    /* see what percentage of the host's screen would be taken up by
       an unscaled emulated framebuffer: */
    percentage = (fb_area * 100) / avail_area;

    /* if this is at least 70%, halve the emulated framebuffer, else
       if this is 30% or less, double the emulated framebuffer: */
    if (percentage >= 70) {
      scale = TME_FB_XLAT_SCALE_HALF;
    }
    else if (percentage <= 30) {
      scale = TME_FB_XLAT_SCALE_DOUBLE;
    }
    else {
      scale = TME_FB_XLAT_SCALE_NONE;
    }

    screen->tme_gtk_screen_fb_scale = -scale;
  }

  /* get the system's default visual: */
  visual = gdk_visual_get_system();

  /* create the new GdkImage for the screen: */
  width = ((conn_fb_other->tme_fb_connection_width
	    * scale)
	   / TME_FB_XLAT_SCALE_NONE);
  height = ((conn_fb_other->tme_fb_connection_height
	     * scale)
	    / TME_FB_XLAT_SCALE_NONE);
  gdkimage = gdk_image_new(GDK_IMAGE_FASTEST,
			   visual,
			   width,
			   height);

  /* set the new image on the image widget: */
  gtk_image_set(GTK_IMAGE(screen->tme_gtk_screen_gtkimage),
		gdkimage,
		NULL);

  /* destroy the previous gdkimage and remember the new one: */
  gdk_image_destroy(screen->tme_gtk_screen_gdkimage);
  screen->tme_gtk_screen_gdkimage = gdkimage;

  /* update our framebuffer connection: */
  conn_fb->tme_fb_connection_width = width;
  conn_fb->tme_fb_connection_height = height;
  conn_fb->tme_fb_connection_depth = gdkimage->depth;
  conn_fb->tme_fb_connection_bits_per_pixel = _tme_gtk_gdkimage_bipp(gdkimage);
  conn_fb->tme_fb_connection_skipx = 0;
  conn_fb->tme_fb_connection_scanline_pad = _tme_gtk_gdkimage_scanline_pad(gdkimage);
  conn_fb->tme_fb_connection_order = (gdkimage->byte_order == GDK_LSB_FIRST
				      ? TME_ENDIAN_LITTLE
				      : TME_ENDIAN_BIG);
  conn_fb->tme_fb_connection_buffer = gdkimage->mem;

  /* when we're showing a framebuffer with a depth of one, we need to
     provide the pixel values for black and white, and, if we're
     halving, three shades of gray in between: */
  if (conn_fb->tme_fb_connection_depth1_map != NULL) {

    /* XXX we almost certainly should free colors
       previously allocated: */
    tme_free(conn_fb->tme_fb_connection_depth1_map);
  }
  if (conn_fb_other->tme_fb_connection_depth == 1) {

    /* allocate the depth-one map: */
    color_count = (scale == TME_FB_XLAT_SCALE_HALF
		   ? 5
		   : 2);
    conn_fb->tme_fb_connection_depth1_map
      = tme_new(tme_uint32_t, color_count);

    /* allocate the colors it needs: */
    for (color_i = 0;
	 color_i < color_count;
	 color_i++) {

      /* set the RGB values: */
      color.red
	= (((65535UL * color_i) / (color_count - 1))
	   ^ screen->tme_gtk_screen_mono_invert_mask);
      color.green = color.red;
      color.blue = color.red;

      /* allocate the color: */
      gdk_colormap_alloc_color(gdk_colormap_get_system(),
			       &color,
			       FALSE,
			       TRUE);
      conn_fb->tme_fb_connection_depth1_map[color_i]
	= color.pixel;
    }
  }
  else {
    conn_fb->tme_fb_connection_depth1_map = NULL;
  }

  /* compose the framebuffer translation question: */
  fb_xlat_q.tme_fb_xlat_width			= conn_fb_other->tme_fb_connection_width;
  fb_xlat_q.tme_fb_xlat_height			= conn_fb_other->tme_fb_connection_height;
  fb_xlat_q.tme_fb_xlat_scale			= (unsigned int) scale;
  fb_xlat_q.tme_fb_xlat_src_depth		= conn_fb_other->tme_fb_connection_depth;
  fb_xlat_q.tme_fb_xlat_src_bits_per_pixel	= conn_fb_other->tme_fb_connection_bits_per_pixel;
  fb_xlat_q.tme_fb_xlat_src_skipx		= conn_fb_other->tme_fb_connection_skipx;
  fb_xlat_q.tme_fb_xlat_src_scanline_pad	= conn_fb_other->tme_fb_connection_scanline_pad;
  fb_xlat_q.tme_fb_xlat_src_order		= conn_fb_other->tme_fb_connection_order;
  fb_xlat_q.tme_fb_xlat_dst_depth		= conn_fb->tme_fb_connection_depth;
  fb_xlat_q.tme_fb_xlat_dst_bits_per_pixel	= conn_fb->tme_fb_connection_bits_per_pixel;
  fb_xlat_q.tme_fb_xlat_dst_skipx		= conn_fb->tme_fb_connection_skipx;
  fb_xlat_q.tme_fb_xlat_dst_scanline_pad	= conn_fb->tme_fb_connection_scanline_pad;
  fb_xlat_q.tme_fb_xlat_dst_order		= conn_fb->tme_fb_connection_order;

  /* ask the framebuffer translation question: */
  fb_xlat_a = tme_fb_xlat_best(&fb_xlat_q);

  /* if this translation isn't optimal, log a note: */
  if (!tme_fb_xlat_is_optimal(fb_xlat_a)) {
    tme_log(&display->tme_gtk_display_element->tme_element_log_handle, 0, TME_OK,
	    (&display->tme_gtk_display_element->tme_element_log_handle,
	     _("no optimal framebuffer translation function available")));
  }

  /* save the translation function: */
  screen->tme_gtk_screen_fb_xlat = fb_xlat_a->tme_fb_xlat_func;

  /* force the next translation to do a complete redraw: */
  tme_fb_xlat_redraw(conn_fb_other);

  /* unlock our mutex: */
  tme_mutex_unlock(&display->tme_gtk_display_mutex);

  /* done: */
  return (TME_OK);
}

/* this sets the screen size: */
static void
_tme_gtk_screen_scale_set(GtkWidget *widget,
			  struct tme_gtk_screen *screen,
			  int scale_new)
{
  struct tme_gtk_display *display;
  int scale_old;
  int rc;

  /* return now if the menu item isn't active: */
  if (!GTK_CHECK_MENU_ITEM(GTK_MENU_ITEM(widget))->active) {
    return;
  }

  /* get the display: */
  display = screen->tme_gtk_screen_display;

  /* lock our mutex: */
  tme_mutex_lock(&display->tme_gtk_display_mutex);

  /* get the old scaling and set the new scaling: */
  scale_old = screen->tme_gtk_screen_fb_scale;
  if (scale_old < 0
      && scale_new < 0) {
    scale_new = scale_old;
  }
  screen->tme_gtk_screen_fb_scale = scale_new;

  /* unlock our mutex: */
  tme_mutex_unlock(&display->tme_gtk_display_mutex);

  /* call the mode change function if the scaling has changed: */
  if (scale_new != scale_old) {
    rc = _tme_gtk_screen_mode_change(screen->tme_gtk_screen_fb);
    assert (rc == TME_OK);
  }
}

/* this sets the screen scaling to default: */
static void
_tme_gtk_screen_scale_default(GtkWidget *widget,
			      struct tme_gtk_screen *screen)
{
  _tme_gtk_screen_scale_set(widget,
			    screen,
			    -TME_FB_XLAT_SCALE_NONE);
}

/* this sets the screen scaling to half: */
static void
_tme_gtk_screen_scale_half(GtkWidget *widget,
			   struct tme_gtk_screen *screen)
{
  _tme_gtk_screen_scale_set(widget,
			    screen,
			    TME_FB_XLAT_SCALE_HALF);
}

/* this sets the screen scaling to none: */
static void
_tme_gtk_screen_scale_none(GtkWidget *widget,
			   struct tme_gtk_screen *screen)
{
  _tme_gtk_screen_scale_set(widget,
			    screen,
			    TME_FB_XLAT_SCALE_NONE);
}

/* this sets the screen scaling to double: */
static void
_tme_gtk_screen_scale_double(GtkWidget *widget,
			     struct tme_gtk_screen *screen)
{
  _tme_gtk_screen_scale_set(widget,
			    screen,
			    TME_FB_XLAT_SCALE_DOUBLE);
}

/* this makes a new screen: */
struct tme_gtk_screen *
_tme_gtk_screen_new(struct tme_gtk_display *display)
{
  struct tme_gtk_screen *screen, **_prev;
  GtkWidget *menu_bar;
  GtkWidget *menu;
  GtkWidget *submenu;
  GtkWidget *menu_item;
  GtkWidget **_menu_item;
  GSList *menu_group;
  tme_uint8_t *bitmap_data;
  unsigned int y;
  const char *menu_label;
  GtkSignalFunc menu_func;
  int i;
#define BLANK_SIDE (16 * 8)

  /* create the new screen and link it in: */
  for (_prev = &display->tme_gtk_display_screens;
       (screen = *_prev) != NULL;
       _prev = &screen->tme_gtk_screen_next);
  screen = *_prev = tme_new0(struct tme_gtk_screen, 1);

  /* the backpointer to the display: */
  screen->tme_gtk_screen_display = display;
  
  /* there is no framebuffer connection yet: */
  screen->tme_gtk_screen_fb = NULL;

  /* the user hasn't specified a scaling yet: */
  screen->tme_gtk_screen_fb_scale
    = -TME_FB_XLAT_SCALE_NONE;

  /* XXX this should be controlled by an argument somewhere: */
  screen->tme_gtk_screen_mono_invert_mask = 0xffff;

  /* create the top-level window, and allow it to shrink, grow,
     and auto-shrink: */
  screen->tme_gtk_screen_window
    = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_policy(GTK_WINDOW(screen->tme_gtk_screen_window),
			TRUE, TRUE, TRUE);

  /* create the outer vertical packing box: */
  screen->tme_gtk_screen_vbox0
    = gtk_vbox_new(FALSE, 0);

  /* add the outer vertical packing box to the window: */
  gtk_container_add(GTK_CONTAINER(screen->tme_gtk_screen_window),
		    screen->tme_gtk_screen_vbox0);

  /* create the menu bar and pack it into the outer vertical packing
     box: */
  menu_bar = gtk_menu_bar_new ();
  gtk_box_pack_start(GTK_BOX(screen->tme_gtk_screen_vbox0), 
		     menu_bar,
		     FALSE, FALSE, 0);
  gtk_widget_show(menu_bar);

  /* create the Screen menu: */
  menu = gtk_menu_new();

  /* create the Screen scaling submenu: */
  submenu = gtk_menu_new();

  /* create the Screen scaling submenu options: */
  menu_group = NULL;
  for (i = 0;; i++) {
    if (i == 0) {
      menu_label = _("Default");
      _menu_item = &screen->tme_gtk_screen_scale_default;
      menu_func = GTK_SIGNAL_FUNC(_tme_gtk_screen_scale_default);
    }
    else if (i == 1) {
      menu_label = _("Half");
      _menu_item = &screen->tme_gtk_screen_scale_half;
      menu_func = GTK_SIGNAL_FUNC(_tme_gtk_screen_scale_half);
    }
    else if (i == 2) {
      menu_label = _("Full");
      _menu_item = NULL;
      menu_func = GTK_SIGNAL_FUNC(_tme_gtk_screen_scale_none);
    }
    else if (i == 3) {
      menu_label = _("Double");
      _menu_item = NULL;
      menu_func = GTK_SIGNAL_FUNC(_tme_gtk_screen_scale_double);
    }
    else {
      break;
    }
    menu_item
      = gtk_radio_menu_item_new_with_label(menu_group,
					   menu_label);
    if (_menu_item != NULL) {
      *_menu_item = menu_item;
    }
    menu_group
      = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
    gtk_signal_connect(GTK_OBJECT(menu_item), 
		       "activate",
		       menu_func,
		       (gpointer) screen);
    gtk_menu_append(GTK_MENU(submenu), menu_item);
    gtk_widget_show(menu_item);
  }

  /* create the Screen scaling submenu item: */
  menu_item = gtk_menu_item_new_with_label(_("Scale"));
  gtk_widget_show(menu_item);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), submenu);
  gtk_menu_append(GTK_MENU(menu), menu_item);

  /* create the Screen menu bar item, attach the menu to it, and 
     attach the menu bar item to the menu bar: */
  menu_item = gtk_menu_item_new_with_label("Screen");
  gtk_widget_show(menu_item);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), menu);
  gtk_menu_bar_append(GTK_MENU_BAR(menu_bar), menu_item);

  /* create an event box for the framebuffer area: */
  screen->tme_gtk_screen_event_box
    = gtk_event_box_new();

  /* pack the event box into the outer vertical packing box: */
  gtk_box_pack_start(GTK_BOX(screen->tme_gtk_screen_vbox0), 
		     screen->tme_gtk_screen_event_box,
		     FALSE, FALSE, 0);

  /* show the event box: */
  gtk_widget_show(screen->tme_gtk_screen_event_box);

  /* create a GdkImage of an alternating-bits area.  we must use
     malloc() here since this memory will end up as part of an XImage,
     and X will call free() on it: */
  bitmap_data = (tme_uint8_t *)
    malloc((BLANK_SIDE * BLANK_SIDE) / 8);
  assert(bitmap_data != NULL);
  for (y = 0;
       y < BLANK_SIDE;
       y++) {
    memset(bitmap_data
	   + (y * BLANK_SIDE / 8),
	   (y & 1
	    ? 0x33
	    : 0xcc),
	   (BLANK_SIDE / 8));
  }
  screen->tme_gtk_screen_gdkimage
    = gdk_image_new_bitmap(gdk_visual_get_system(),
			   bitmap_data,
			   BLANK_SIDE,
			   BLANK_SIDE);

  /* create the GtkImage for the framebuffer area: */
  screen->tme_gtk_screen_gtkimage
    = gtk_image_new_from_image(screen->tme_gtk_screen_gdkimage, NULL);

  /* add the GtkImage to the event box: */
  gtk_container_add(GTK_CONTAINER(screen->tme_gtk_screen_event_box), 
		    screen->tme_gtk_screen_gtkimage);

  /* show the GtkImage: */
  gtk_widget_show(screen->tme_gtk_screen_gtkimage);

  /* show the outer vertical packing box: */
  gtk_widget_show(screen->tme_gtk_screen_vbox0);

  /* show the top-level window: */
  gtk_widget_show(screen->tme_gtk_screen_window);

  /* there is no translation function: */
  screen->tme_gtk_screen_fb_xlat = NULL;

  /* attach the mouse to this screen: */
  _tme_gtk_mouse_attach(screen);

  /* attach the keyboard to this screen: */
  _tme_gtk_keyboard_attach(screen);

  return (screen);
}

/* this breaks a framebuffer connection: */
static int
_tme_gtk_screen_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

/* this makes a new framebuffer connection: */
static int
_tme_gtk_screen_connection_make(struct tme_connection *conn,
				unsigned int state)
{
  struct tme_gtk_display *display;
  struct tme_gtk_screen *screen;
  struct tme_fb_connection *conn_fb;
  struct tme_fb_connection *conn_fb_other;

  /* recover our data structures: */
  display = (struct tme_gtk_display *) conn->tme_connection_element->tme_element_private;
  conn_fb = (struct tme_fb_connection *) conn;
  conn_fb_other = (struct tme_fb_connection *) conn->tme_connection_other;

  /* both sides must be framebuffer connections: */
  assert(conn->tme_connection_type
	 == TME_CONNECTION_FRAMEBUFFER);
  assert(conn->tme_connection_other->tme_connection_type
	 == TME_CONNECTION_FRAMEBUFFER);

  /* we're always set up to answer calls across the connection, so we
     only have to do work when the connection has gone full, namely
     taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* lock our mutex: */
    tme_mutex_lock(&display->tme_gtk_display_mutex);

    /* if our initial screen is already connected, make a new screen: */
    screen = display->tme_gtk_display_screens;
    if (screen->tme_gtk_screen_fb != NULL) {
      screen = _tme_gtk_screen_new(display);
    }

    /* save our connection: */
    screen->tme_gtk_screen_fb = conn_fb;

    /* unlock our mutex: */
    tme_mutex_unlock(&display->tme_gtk_display_mutex);

    /* call our mode change function: */
    _tme_gtk_screen_mode_change(conn_fb);
  }

  return (TME_OK);
}

/* this makes a new connection side for a GTK screen: */
int
_tme_gtk_screen_connections_new(struct tme_gtk_display *display, 
				struct tme_connection **_conns)
{
  struct tme_fb_connection *conn_fb;
  struct tme_connection *conn;

  /* allocate a new framebuffer connection: */
  conn_fb = tme_new0(struct tme_fb_connection, 1);
  conn = &conn_fb->tme_fb_connection;
  
  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_FRAMEBUFFER;
  conn->tme_connection_score = tme_fb_connection_score;
  conn->tme_connection_make = _tme_gtk_screen_connection_make;
  conn->tme_connection_break = _tme_gtk_screen_connection_break;

  /* fill in the framebuffer connection: */
  conn_fb->tme_fb_connection_mode_change = _tme_gtk_screen_mode_change;

  /* return the connection side possibility: */
  *_conns = conn;

  /* done: */
  return (TME_OK);
}
