
#include <R.h> //for errors
#include <R_ext/Utils.h> //for interrupt

void taxaTrim(char **files){
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
    if(nTabs==1||(nTabs==2&&byte!='\t'))fputc(byte,out);
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
