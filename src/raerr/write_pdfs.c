/* write_pdfs
 *
 * Write the rain rate PDFs for every pixel in a radar image to a NetCDF file.
 *
 * Tim Hume.
 * 21 September 2007.
 */

#include "radar_error.h"

int write_pdfs(char *infile, char *outfile, struct radar *radar_data){
	int		ncid;						/* ID of the output NetCDF file.														*/
	int		in_ncid;					/* ID of the input NetCDF file.															*/

	int		dimids[3];					/* IDs of up to three dimensions. It is OK to use less than 3 dimensions.				*/
	size_t	start[3];					/* Starting point of a slab of data with up to 3 dimensions.							*/
	size_t	count[3];					/* Size of a slab of data with up to 3 dimensions.										*/
	double	percentiles[100];			/* Holds the percentiles.																*/

	int		varid;						/* Variable ID.																			*/
	int		lat_varid;					/* ID of the latitude variable.															*/
	int		lon_varid;					/* ID of the longitude variable.														*/
	int		neareststn_varid;			/* ID of the variable containing the closest station number.							*/
	int		percentile_varid;			/* ID of the percentile variable ID.													*/
	int		rainrate_varid;				/* ID of the rain rate variable.														*/
	int		rainratepdf_varid;			/* ID of the rain rate PDF variable.													*/
	double	missing_value;				/* Missing value attribute.																*/

	size_t	ii;							/* Counter.																				*/
	int		nc_status;					/* Return value from NetCDF functions.													*/
	int		return_status = 0;

	/*
	 * Open the file for writing.
	 */
	if ((nc_status = nc_create(outfile, NC_WRITE, &ncid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot create %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	/*
	 * Define the dimensions in the file.
	 */
	if ((nc_status = nc_def_dim(ncid, "i", radar_data->nx, &dimids[0])) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the i dimension in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_dim(ncid, "j", radar_data->nx, &dimids[1])) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the j dimension in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_dim(ncid, "percentile", 100, &dimids[2])) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the percentile dimension in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	/*
	 * Define the variables in the file.
	 */
	if ((nc_status = nc_def_var(ncid, "lon", NC_DOUBLE, 2, dimids, &lon_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the lon variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_var(ncid, "lat", NC_DOUBLE, 2, dimids, &lat_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the lat variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_var(ncid, "nearest_stn", NC_INT, 2, dimids, &neareststn_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the nearest_stn variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_var(ncid, "percentile", NC_DOUBLE, 1, (dimids+2), &percentile_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the percentile variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_var(ncid, "rain_rate", NC_DOUBLE, 2, dimids, &rainrate_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the rain_rate variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}
	if ((nc_status = nc_def_var(ncid, "rain_rate_pdf", NC_DOUBLE, 3, dimids, &rainratepdf_varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot define the rain_rate_pdf variable in %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	/*
	 * Copy the attributes from the input file to the output file.
	 */
	if ((nc_status = nc_open(infile, NC_NOWRITE, &in_ncid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot open %s: %s\n",infile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_inq_varid(in_ncid, "rain_rate", &varid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot get rain_rate variable ID in %s: %s\n",infile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_get_att_double(in_ncid, varid, "missing_value", &missing_value)) != NC_NOERR){
		fprintf(stderr,"E: Cannot read missing value attribute for rain_rate in %s: %s\n",infile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_att_double(ncid, rainrate_varid, "missing_value", NC_DOUBLE, 1, &missing_value)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write missing_value attribute for rain_rate to %s: %s\n", outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_att_double(ncid, rainratepdf_varid, "missing_value", NC_DOUBLE, 1, &missing_value)) 
			!= NC_NOERR){
		fprintf(stderr,"E: Cannot write missing_value attribute for rain_rate_pdf to %s: %s\n", outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_att_text(ncid, percentile_varid, "long_name", 11, "percentile\0")) != NC_NOERR){
		fprintf(stderr,"E: Cannot write long_name attribute for percentile to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_att_text(ncid, percentile_varid, "units", 2, "%\0")) != NC_NOERR){
		fprintf(stderr,"E: Cannot write long_name attribute for percentile to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_close(in_ncid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot close %s: %s\n",infile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	/*
	 * Write the data to the file.
	 */
	if ((nc_status = nc_enddef(ncid)) != NC_NOERR){
		fprintf(stderr,"E: %s\n",nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	for (ii=0; ii<(radar_data->nx * radar_data->ny); ++ii){
		start[0]	= ii/radar_data->ny;
		start[1]	= ii%radar_data->ny;
		start[2]	= 0;
		count[0]	= 1;
		count[1]	= 1;
		count[2]	= 100;
		if ((nc_status = nc_put_vara_double(ncid, rainratepdf_varid, start, count, *(radar_data->rain_rate_pdf+ii))) != NC_NOERR){
			fprintf(stderr,"E: Cannot write rain_rate_pdf data to %s: %s\n",outfile, nc_strerror(nc_status));
			return_status = 1;
			goto finish;
		}
	}

	if ((nc_status = nc_put_var_double(ncid, rainrate_varid, radar_data->rain_rate)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write rain_rate data to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_var_double(ncid, lon_varid, radar_data->lon)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write lon data to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_var_int(ncid, neareststn_varid, radar_data->nearest_stn)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write nearest_stn data to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	if ((nc_status = nc_put_var_double(ncid, lat_varid, radar_data->lat)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write lat data to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

	for (ii=0; ii<100; ++ii) percentiles[ii] = 99.5 - (double)ii;
	if ((nc_status = nc_put_var_double(ncid, percentile_varid, percentiles)) != NC_NOERR){
		fprintf(stderr,"E: Cannot write percentiles data to %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
		goto finish;
	}

finish:
	/*
	 * Close the file.
	 */
	if ((nc_status = nc_close(ncid)) != NC_NOERR){
		fprintf(stderr,"E: Cannot close %s: %s\n",outfile, nc_strerror(nc_status));
		return_status = 1;
	}

	return return_status;
}
