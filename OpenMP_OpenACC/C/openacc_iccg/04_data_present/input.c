/*
 * INPUT
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "struct_ext.h"
#include "pcg_ext.h"
#include "input.h"

extern int
INPUT(void)
{
#define BUF_SIZE 1024
	char line[BUF_SIZE];
	char CNTFIL[81];
	double OMEGA;
	FILE *fp11;

/**************
 * CNTL. file *
 **************/
	if((fp11 = fopen("INPUT.DAT", "r")) == NULL) {
		fprintf(stderr, "Error: %s\n", strerror(errno));
		return -1;
	}

	fgets(line, BUF_SIZE, fp11);
	sscanf(line, "%d%d%d", &NX, &NY, &NZ);
	fgets(line, BUF_SIZE, fp11);
	sscanf(line, "%le%le%le", &DX, &DY, &DZ);
	fgets(line, BUF_SIZE, fp11);
	sscanf(line, "%le", &EPSICCG);
	fgets(line, BUF_SIZE, fp11);
	sscanf(line, "%d", &PEsmpTOT);
	fgets(line, BUF_SIZE, fp11);
	sscanf(line, "%d", &NCOLORtot);
	
	fprintf(stderr, "\n### THREAD number=%8d\n", PEsmpTOT);

	fclose(fp11);


	return 0;
}


extern int
INPUT_COMMANDLINE(int argc, char*argv[])
{

  /**************
   * from command line arguments.*
   **************/

#define BUF_SIZE 1024
  char line[BUF_SIZE];
  char CNTFIL[81];
  FILE *fp11;

  /**************
   * CNTL. file *
   **************/
  if((fp11 = fopen("INPUT.DAT", "r")) == NULL) {
    fprintf(stderr, "Error: %s\n", strerror(errno));
    return -1;
  }

  fgets(line, BUF_SIZE, fp11);
  sscanf(line, "%d%d%d", &NX, &NY, &NZ);
  fgets(line, BUF_SIZE, fp11);
  sscanf(line, "%le%le%le", &DX, &DY, &DZ);
  fgets(line, BUF_SIZE, fp11);
  sscanf(line, "%le", &EPSICCG);
  fgets(line, BUF_SIZE, fp11);
  sscanf(line, "%d", &PEsmpTOT);
  fgets(line, BUF_SIZE, fp11);
  sscanf(line, "%d", &NCOLORtot);

  fclose(fp11);

  int i;
  for(i = 1;i < argc;i++){
    if(strcmp(argv[i],"-nx") == 0 || strcmp(argv[i],"-NX") == 0){
      i++;
      NX = atoi(argv[i]);
    }
    if(strcmp(argv[i],"-ny") == 0 || strcmp(argv[i],"-NY") == 0){
      i++;
      NY = atoi(argv[i]);
    }
    if(strcmp(argv[i],"-nz") == 0 || strcmp(argv[i],"-NZ") == 0){
      i++;
      NZ = atoi(argv[i]);
    }
    if(strcmp(argv[i],"-n") == 0 || strcmp(argv[i],"-N") == 0){
      i++;
      NX = atoi(argv[i]);
      NY = atoi(argv[i]);
      NZ = atoi(argv[i]);
    }
    if(strcmp(argv[i],"-dx") == 0 || strcmp(argv[i],"-DX") == 0){
      i++;
      DX = atof(argv[i]);
    }
    if(strcmp(argv[i],"-dy") == 0 || strcmp(argv[i],"-DY") == 0){
      i++;
      DY = atof(argv[i]);
    }
    if(strcmp(argv[i],"-dz") == 0 || strcmp(argv[i],"-DZ") == 0){
      i++;
      DZ = atof(argv[i]);
    }
    if(strcmp(argv[i],"-e") == 0 || strcmp(argv[i],"-E") == 0
       || strcmp(argv[i],"-eps") == 0 || strcmp(argv[i],"-EPS") == 0
       || strcmp(argv[i],"-epsiccg") == 0 || strcmp(argv[i],"-EPSICCG") == 0
       || strcmp(argv[i],"-err") == 0 || strcmp(argv[i],"-ERR") == 0){
      i++;
      EPSICCG = atof(argv[i]);
    }
    if(strcmp(argv[i],"-PEsmpTOT") == 0 || strcmp(argv[i],"-nt") == 0){
      i++;
      PEsmpTOT = atoi(argv[i]);
    }
    if(strcmp(argv[i],"-NCOLORtot") == 0 || strcmp(argv[i],"-ncolor") == 0
       || strcmp(argv[i],"-nc") == 0 || strcmp(argv[i],"-c") == 0 || strcmp(argv[i],"-C") == 0
       || strcmp(argv[i],"-color") == 0 || strcmp(argv[i],"-COLOR") == 0 ){
      i++;
      NCOLORtot = atoi(argv[i]);
    }

  }
  NXin = NX;
  NYin = NY;
  NZin = NZ;
  NCin = NCOLORtot;
  fprintf(stderr, "\n### THREAD number=%8d\n", PEsmpTOT);

  return 0;
}
