/* label.h - assembler-specific label characters for bcc */

/* Copyright (C) 1992 Bruce Evans */

/* defaults */

#define CCNAMEPREFIX '_'
#undef LABELENDCHAR 	        /* last char of ALL labels */
#define LABELSTARTCHAR 'L'	/* first char of names of generated labels */
#define LOCALSTARTCHAR '@'	/* first char of local names */
#define PUBLICENDCHAR ':'
