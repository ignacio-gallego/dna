# DNA Code Challenge Project

Capgemini code challenge on a problem of DNA sequencing.

# Introduction

The sequence of bases in DNA carries the genetic code of all living things on earth. Scattered along the DNA molecule are particularly important sequences of bases known as genes. Each gene is a coded description for making a particular protein. Getting from the code in DNA to the final protein is a very complicated process which will be naively simplified in the following (for a proper deep dive see: https://en.wikipedia.org/wiki/Protein_biosynthesis)
The genetic code in DNA is first transcribed ("copied") to messenger RNA (mRNA). That then travels out of the nucleus of the cell (where the DNA is found) into the cytoplasm of the cell. The cytoplasm contains essentially everything else in the cell apart from the nucleus. Here the code is read, and the protein is synthesised in the ribosome.
This sequence of mRNA is broken into a series of three-nucleotide units known as “codons” (see down). The three-letter nature of codons means that the four nucleotides found in mRNA — called A, U, G, and C — can produce a total of 64 different combinations.
So, a sequence of these three letters, the codons, are typically depicted like the following example:

... CUG UCU AUG GGA AAA UGC UGA UUA AGU UUU AUG UCC UCC ...

Genes are defined by a sequence of what could be many hundreds of codons. Each gene contains a 'start codon' which indicates where it begins and a 'stop codon' which defines where the gene ends. Start codons can vary according to the species but there are commonly three possible stop codons: UAG, UGA and UAA.

https://docs.haskellstack.org/en/stable/README/


# Compiling and running the project

First, download the Stack Haskell compile tool for Windows at: https://docs.haskellstack.org/en/stable/README/

Then, step on the main folder of the project, and build it with the following command:

```stack build'''

This will generate a Windows .exe file. If you like, you can run this file. Alterantively, to run the project enter the following Stack command and press ENTER:

```stack run''

