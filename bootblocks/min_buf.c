
#include "monitor.h"

#ifdef MINI_BUF
#ifndef MAXTRK
#define MAXTRK	21
#endif

int disk_drive = 0;
int disk_spt   = 7;
int disk_heads = 2;
int disk_cyls  = 0;
int bad_track  = -1;

static int track_no = -1;
static char buffer[MAXTRK*512];

void reset_disk()
{
   disk_spt   = 7;
   disk_heads = 2;
   disk_cyls  = 0;
   bad_track  = -1;
}

char * read_lsector(sectno)
long sectno;
{
   int tries = 5;
   int rv = 0;

   int phy_s = 1;
   int phy_h = 0;
   int phy_c = 0; 
   int ltrack;

   if( sectno == 0 ) reset_disk();

   if( disk_spt < 1 || disk_heads < 1 )
      phy_s = sectno;
   else
   {
      phy_s = sectno%disk_spt;
      phy_h = sectno/disk_spt%disk_heads;
      phy_c = sectno/disk_spt/disk_heads;
   }

#ifdef __ELKS__
   fprintf(stderr, "read_sector(%ld = %d,%d,%d)\n", sectno, phy_c, phy_h, phy_s);
#endif

   ltrack = phy_c*disk_heads+phy_h;
   if( disk_spt > 7 && disk_spt <= MAXTRK
    && track_no != ltrack && ltrack != bad_track)
   {
     rv = phy_read(disk_drive, phy_c, phy_h, 1, disk_spt, buffer);
     if( rv == 0 )
        track_no = ltrack;
     else
        bad_track = ltrack;
   }
   if( track_no == phy_c*disk_heads+phy_h )
      return buffer + phy_s * 512;

   do
   {
     rv = phy_read(disk_drive, phy_c, phy_h, phy_s+1, 1, buffer);
     tries--;
     if( rv ) printf("Error in phy_read(%d,%d,%d,%d,%d,%d);\n",
		      disk_drive, phy_c, phy_h, phy_s+1, 1, buffer);
   }
   while(rv && tries > 0);

   if(rv) return 0; else return buffer;
}

#if defined(__MSDOS__) || defined(__STANDALONE__)
phy_read(drive, cyl, head, sect, length, buffer)
{
#asm
  push	bp
  mov	bp,sp

  push	ds
  pop	es

  mov	dl,[bp+2+_phy_read.drive]
  mov	ch,[bp+2+_phy_read.cyl]
  mov	dh,[bp+2+_phy_read.head]
  mov	cl,[bp+2+_phy_read.sect]
  mov	al,[bp+2+_phy_read.length]
  mov	bx,[bp+2+_phy_read.buffer]

  mov	ah,#$02
  int	$13
  jc	read_err
  mov	ax,#0
read_err:

  pop	bp
#endasm
}
#endif

#endif
