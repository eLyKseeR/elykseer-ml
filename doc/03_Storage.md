[Content](01_Content.md)

# Details about data storage

File content is copied into an assembly in a non-local way, i.e. bytes that follow each other in the original file are stored far apart in the assembly. As the assembly gets output to chunks they even end up in different files on disk.

![Data storage in eLyKseeR](./img/img3.png)

The image shows on the left a file with its content color labelled by position and on the right a schema of an assembly.
The reading order in the file is from left to right in a row oriented manner. The colors repeat: after purple, blue continues.
As the data is copied from the file, it is added to the assembly in a column oriented manner.
This means that the first datum (blue) is added to the beginning of the first chunk. The second datum (teal) is added to the beginning of the second chunk. The third datum (green) ends up in the third chunk and so on.

In other words: adjacent bytes from the file *b<sub>i</sub>* and *b<sub>i+1</sub>* become *B<sub>k</sub>* and *B<sub>k+(256\*1024)</sub>* so are definitely not adjacent anymore. Their indizes are spaced apart by the size of a chunk (256*1024 bytes).
