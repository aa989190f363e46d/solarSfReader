library('digest') ## for sha256 from data file.
library('RSQLite')
library('bitops') ## for bitwise hashes XOR. binXor.

source('./storages.r')
source('./calculations.r')
source('./readSpc.r')
source('./manipulator.r')