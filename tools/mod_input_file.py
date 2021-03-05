#!/use/bin/env python


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- mod_input_file.py
#--
## @brief Python class input_file is the dictionary associating juRS keywords and values
#  @author Shigeru Tsukamoto (s.tsukamoto@fz-juelich.de)
class input_file:


    ## @brief Constant string list of juRS block keywords
    __BLOCKKEYS = { "kpoints", "kpath", "atoms", "atoms_fractional" }


    #----1---------2---------3---------4---------5---------6---------7---------8---------9---------0
    #--
    ## @brief Constructor of class input_file
    #  @param[in] filename name of file to be imported. File must be juRS input file
    #  @return    Python dictionary of pairs of input keywords and values
    def __init__( self, filename ):


        #-- Import module
        import os.path


        #-- Set parameter
        PROMPT = __name__.upper() + " >>>"


        #-- Display argument
        print( "%s filename = %s" % ( PROMPT, filename ) )


        #-- Check existence of the file
        if not os.path.isfile( filename ):
            raise Exception( "%s ERROR: input file not found. filename = %s" % ( PROMPT, filename ) )


        #-- Open file and read the first HEADERLENGTH bytes
        with open( filename, "r" ) as fp:
            lines = [ line.strip().partition("#")[0] for line in fp.readlines() ]


        #-- Remove null item from lines
        while lines.count( "" ) > 0:
            lines.remove( "" )


        #-- Scan bufs and find key & value
        #-- Construct header class
        i = 0
        while i < len( lines ):
            if lines[i] in input_file.__BLOCKKEYS:
                key = lines[i]
                self.__dict__[ key ] = []
                i += 1
                while True:
                    if lines[i] == key:
                        i += 1
                        break
                    self.__dict__[ key ].append( lines[i] )
                    i += 1
            else:
                print lines[i]
                ( key, value ) = lines[i].split( None, 1 )
                self.__dict__[ key ] = value
                i += 1


        return


    #--
    #-- Get
    #--
    def get( self, key ):
        return self.__dict__[ key ]


    #--
    #-- Exist
    #--
    def exist( self, key ):
        return key in self.__dict__


#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0


#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
#--
#-- Main routine for test
#--
if __name__ == "__main__":

    import sys

    filename = sys.argv[1]

    test = input_file( filename )
    print test.__dict__
    print test.exist( "cell_size" )
    print test.get( "cell_size" )
    print test.exist( "cell" )
    print test.get( "cell" )

    sys.exit( 0 )

#--
#---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0
