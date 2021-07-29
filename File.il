#include "coreassembly.h"

.assembly Sabre
{
  .ver 1:0:0:0
}
.namespace Sabre{
.module Sabre.dll
.class public auto ansi beforefieldinit Messages
  extends [CORE_ASSEMBLY]System.Object
{
  .method public hidebysig static string Print() cil managed
  {
    ldstr "Yeehaw!"
    ret
  }
}
}
