# Purpose

To investigate logs in a fairly crude way. Clusters lines in a file based on jaccard distance
Works fairly well on logs, because they're pretty regular

# Limitations
## Not very optimised

Lot of lists, instead of vectors, and some quadratic complexity
(removable with Tekmo's fold library?)

## Number of clusters is a function of max distance

But the representative element of a cluster is selected greedily, and
only it determines whether a new line belongs in a cluster based on
the candidate rather than the centroid

# Pathological issues

## Single fields that contain the field seperator

A bytestring printed as e.g. `43 8B 23 00 1B 28 41` is going to count as
7 seperate words, increasing significantly the distance between
otherwise similar lines

# TODO

* Add some colours?
* Enhance the flags somewhat
  * Specify fields or words or char ranges to ignore?
  * AWK style record seperator and field separator?
  * Add a flag for only showing the cluster heads
  * Allow sorting by sum of entropies in a field
