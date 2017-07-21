LIBLMDB_OBJECTS = mdb.o midl.o

liblmdb.a: $(LIBLMDB_OBJECTS)
	$(AR) rs $@ mdb.o midl.o
mdb.o: mdb.c lmdb.h midl.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c mdb.c
midl.o: midl.c midl.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -c midl.c
clean:
	rm -f $(LIBLMDB_OBJECTS) liblmdb.a
