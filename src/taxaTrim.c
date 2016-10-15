
#include <R.h>
#include <zlib.h>

void taxaTrim(char **files){
  int bytes;
  unsigned int nTabs=0;
  unsigned int line=1;
  unsigned int pos=0;
  char byte;
  FILE *out;
  gzFile in;
  in=gzopen(files[0],"r");
  if(!in)error("COuld not open in file");
  out=fopen(files[1],"a");
  if(!out)error("COuld not open out file");
  //delete first line
  while(byte = gzgetc(in)&&byte!='\n')next();
  while(byte = gzgetc(in)){
    if(nTabs==1||(nTabs==2&&byte!='\t'))fputc(byte,out);
    if(byte=='\t')nTabs++;
    if(byte=='\n'){
      if(nTabs!=3&pos!=0)error("Malformed line on line %d ",line);
      fputc('\n',out);
      nTabs=0;
      line++;
      pos=0;
    }
    pos++;
  }
  fclose(out);
  gzclose(in);
}
