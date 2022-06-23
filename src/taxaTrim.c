
#include <R.h> //for errors
#include <R_ext/Utils.h> //for interrupt

void safe_fputc(int ch, FILE *stream){
  //char byte;
  //if((byte=fputc(ch,stream)) != ch || ferror(stream) !=0)error("Could not write to file. Out of disk space?");
  //Rprintf("Chars: %d %d %d %d %d\n",ch,byte,ferror(stream),fflush(stream),ferror(stream));
  if(fputc(ch,stream) != ch || ferror(stream) !=0)error("Could not write to file. Out of disk space?");
}

void taxaTrim(char **files,int *desiredCols, int *nCol){
  unsigned int nTabs=0;
  unsigned int line=1;
  unsigned int pos=0;
  char byte;
  FILE *out;
  FILE *in;
  in=fopen(files[0],"r");
  if(!in)error("Could not open input file");
  out=fopen(files[1],"a");
  if(!out)error("Could not open output file");
  //delete first line
  while((byte = fgetc(in))!=EOF){
    if(byte=='\n')break;
  }
  while((byte = fgetc(in))!=EOF){
    for(int ii=0;ii<nCol[0];ii++){
      if(nTabs==desiredCols[ii]){
        if((byte!='\t') && (byte!='\n'))safe_fputc(byte,out);
        else if(ii<nCol[0]-1)safe_fputc('\t',out);
        break;
      }
    }
    if(byte=='\t')nTabs++;
    if(byte=='\n'){
      if((nTabs!=3) && (pos!=0))error("Malformed line on line %d ",line+1);
      safe_fputc('\n',out);
      nTabs=0;
      line++;
      pos=0;
    }else{
      pos++;
    }
    if(line%256==0)R_CheckUserInterrupt();
  }
  //check last line
  if((nTabs!=3) && (pos!=0))error("Malformed line on line %d ",line+1);
  //might not catch problems earlier if not flushed to disk
  if(fflush(out)!=0 || ferror(out)!=0)error("Could not write to file. Out of disk space?");
  fclose(out);
  fclose(in);
}
