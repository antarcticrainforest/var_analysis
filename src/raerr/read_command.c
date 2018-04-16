/* read_command
 *
 * Read the radar_error command line.
 *
 * Tim Hume.
 * 20 September 2007.
 */

#include "radar_error.h"

int read_command(int argc, char *argv[], struct cli *cmdline){
   int      option;
   int      return_status   = 0;

   cmdline->infile            = NULL;
   cmdline->errorstats_file   = NULL;
   cmdline->outfile         = NULL;

   while ((option = getopt(argc, argv, "i:e:o:")) != -1){
      switch (option){
         case 'i':
            if (!(cmdline->infile = realloc(cmdline->infile, (strlen(optarg)+1)*sizeof(char)))){
               fprintf(stderr,"E: Memory allocation error.\n");
               return_status   = 1;
               goto finish;
            }
            strncpy(cmdline->infile, optarg, (strlen(optarg)+1));
            break;
         case 'e':
            if (!(cmdline->errorstats_file = realloc(cmdline->errorstats_file, (strlen(optarg)+1)*sizeof(char)))){
               fprintf(stderr,"E: Memory allocation error.\n");
               return_status   = 1;
               goto finish;
            }
            strncpy(cmdline->errorstats_file, optarg, (strlen(optarg)+1));
            break;
         case 'o':
            if (!(cmdline->outfile = realloc(cmdline->outfile, (strlen(optarg)+1)*sizeof(char)))){
               fprintf(stderr,"E: Memory allocation error.\n");
               return_status   = 1;
               goto finish;
            }
            strncpy(cmdline->outfile, optarg, (strlen(optarg)+1));
            break;
         case '?':
            if (isprint(optopt)){
               fprintf(stderr,"E: Unknown option `-%c'.\n",optopt);
            } else {
               fprintf(stderr,"E: Unknown option charcater `\\x%x'.\n",optopt);
            }
            return_status   = 2;
            goto finish;
         default:
            fprintf(stderr,"E: Unable to parse the command line.\n");
            return_status   = 2;
            goto finish;
      }
   }

   /*
    * Check if the required input was provided.
    */
   if (cmdline->infile == NULL){
      fprintf(stderr,"E: An input file must be specified.\n");
      return_status = 2;
      goto finish;
   }

   if (cmdline->errorstats_file == NULL){
      fprintf(stderr,"E: An error statistics file must be specified.\n");
      return_status = 2;
      goto finish;
   }

   if (cmdline->outfile == NULL){
      fprintf(stderr,"E: An output file must be specified.\n");
      return_status = 2;
      goto finish;
   }

   finish:
   return return_status;
}


