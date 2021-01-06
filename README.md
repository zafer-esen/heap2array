Heap2Array
========

Heap2Array takes inputs in SMT-LIB v2.6 that contain heap theory declarations
and operations, and encodes them using the theory of arrays.

Heap2Array depends on Princess: http://www.philipp.ruemmer.org/princess.shtml
Its lineariser is a slight modification of Princess SMTLineariser.

Documentation
-------------

You can either download a binary release of Heap2Array, or compile the Scala
code yourself. Since Eldarica uses <code>sbt</code>, compilation is quite
simple: you just need <code>sbt</code> installed on your machine,
and then type <code>sbt assembly</code> to download the compiler, all
required libraries, and produce a binary of Heap2Array.

After compilation (or downloading a binary release), calling Heap2Array
is normally as easy as saying

  <code>./heap2array input.smt2 </code>

When using a binary release, one can instead also call

  <code>java -jar target/scala-2.\*/heap2array-assembly\*.jar input.smt2</code>

A full list of options can be obtained by calling <code>./heap2array
</code>.<br> 

If more than one input file is passed along with a single output file name, the
specified output file name will be prefixed with an index for each input.

Papers
------

* An extended abstract introducing the theory of heap:
  "Towards an SMT-LIB Theory of Heap"
  https://arxiv.org/html/2008.02483v1/#EPTCS320.12
