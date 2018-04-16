/* read_radar_data
 *
 * Read the radar rain rate data from a NetCDF file.
 *
 * Tim Hume.
 * 20 September 2007.
 */

#include "radar_error.h"

int read_radar_data(char *infile, struct radar *radar_data){
   int      return_status   = 0;
   int      nc_status;
   size_t   ii;                  /* Counter.                                                               */

   int      ncid;               /* ID of the NetCDF file.                                                   */
   int      i_dimid;
   int      j_dimid;
   int      lat_varid;
   int      lon_varid;
   int      rainrate_varid;

   /*
    * Open the file.
    */
   if ((nc_status = nc_open(infile, NC_NOWRITE, &ncid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot open %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   /*
    * Get the size of the dimensions.
    */
   if ((nc_status = nc_inq_dimid(ncid, "i", &i_dimid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot find i dimension: %s\n",nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_inq_dimid(ncid, "j", &j_dimid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot find j dimension: %s\n",nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_inq_dimlen(ncid, i_dimid, &radar_data->nx)) != NC_NOERR){
      fprintf(stderr,"E: Cannot get i dimension length: %s\n",nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_inq_dimlen(ncid, j_dimid, &radar_data->ny)) != NC_NOERR){
      fprintf(stderr,"E: Cannot get j dimension length: %s\n",nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   /*
    * Allocate memory for the various data arrays.
    */
   if (!(radar_data->rain_rate = malloc(radar_data->nx * radar_data->ny * sizeof(double)))){
      fprintf(stderr,"Memory allocation error.\n");
      return_status = 1;
      goto finish;
   }

   if (!(radar_data->lon = malloc(radar_data->nx * radar_data->ny * sizeof(double)))){
      fprintf(stderr,"Memory allocation error.\n");
      return_status = 1;
      goto finish;
   }

   if (!(radar_data->lat = malloc(radar_data->nx * radar_data->ny * sizeof(double)))){
      fprintf(stderr,"Memory allocation error.\n");
      return_status = 1;
      goto finish;
   }

   if (!(radar_data->nearest_stn = malloc(radar_data->nx * radar_data->ny * sizeof(int)))){
      fprintf(stderr,"Memory allocation error.\n");
      return_status = 1;
      goto finish;
   }

   if (!(radar_data->rain_rate_pdf = malloc(radar_data->nx * radar_data->ny * sizeof(double*)))){
      fprintf(stderr,"Memory allocation error.\n");
      return_status = 1;
      goto finish;
   }
   for (ii=0; ii<radar_data->nx * radar_data->ny; ++ii){
      *(radar_data->rain_rate_pdf + ii) = NULL;
      if (!(*(radar_data->rain_rate_pdf + ii) = malloc(100*sizeof(double)))){
         fprintf(stderr,"Memory allocation error.\n");
         return_status = 1;
         goto finish;
      }
   }

   /*
    * Read the relevant data.
    */
   if ((nc_status = nc_inq_varid(ncid, "lon", &lon_varid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot find lon variable in %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_inq_varid(ncid, "lat", &lat_varid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot find lat variable in %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_inq_varid(ncid, "rain_rate", &rainrate_varid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot find rain_rate variable in %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_get_var_double(ncid, lon_varid, radar_data->lon)) != NC_NOERR){
      fprintf(stderr,"E: Cannot read lon from %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_get_var_double(ncid, lat_varid, radar_data->lat)) != NC_NOERR){
      fprintf(stderr,"E: Cannot read lat from %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   if ((nc_status = nc_get_var_double(ncid, rainrate_varid, radar_data->rain_rate)) != NC_NOERR){
      fprintf(stderr,"E: Cannot read rain_rate from %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
      goto finish;
   }

   finish:

   if ((nc_status = nc_close(ncid)) != NC_NOERR){
      fprintf(stderr,"E: Cannot close %s: %s\n",infile, nc_strerror(nc_status));
      return_status = 1;
   }

   return return_status;
}
