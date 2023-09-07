%{
#include <stdio.h>
%}

%%
'[^']'             |
'\\[nt\ \\]\'      | 
\?\\\\[nt\ \\]     |
\?\\[^nt\ \\]      printf("CHAR3 [%s]\n", yytext);
%%

   
