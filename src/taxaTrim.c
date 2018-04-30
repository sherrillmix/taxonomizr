
#include <R.h> //for errors
#include <R_ext/Utils.h> //for interrupt

void taxaTrim(char **files,int *desiredCols, int *nCol){
  unsigned int nTabs=0;
  unsigned int line=1;
  unsigned int pos=0;
  char byte;
  FILE *out;
  FILE *in;
  in=fopen(files[0],"r");
  if(!in)error("Could not open in file");
  out=fopen(files[1],"a");
  if(!out)error("Could not open out file");
  //delete first line
  while((byte = fgetc(in))!=EOF){
    if(byte=='\n')break;
  }
  while((byte = fgetc(in))!=EOF){
    for(int ii=0;ii<nCol[0];ii++){
      if(nTabs==desiredCols[ii]){
        if((byte!='\t') & (byte!='\n'))fputc(byte,out);
        else if(ii<nCol[0]-1)fputc('\t',out);
        break;
      }
    }
    if(byte=='\t')nTabs++;
    if(byte=='\n'){
      if((nTabs!=3)&(pos!=0))error("Malformed line on line %d ",line);
      fputc('\n',out);
      nTabs=0;
      line++;
      pos=0;
    }
    if(line%256==0)R_CheckUserInterrupt();
    pos++;
  }
  fclose(out);
  fclose(in);
}
