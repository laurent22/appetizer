/** @file
   A simple file encryption utility based on the blowfish encryption algorithm.

   @author  Ivan Vecerina - 2002 - This source file is hereby placed in the public domain
            Credits given in derivative works would be appreciated, although not required.

Usage:
 To encrypt a file:  coopfish.exe  [key]  [EncryptedOutputFile]  [InputFile]
 To decrypt a file:  coopfish.exe  [key]  [EncryptedInputFile]

 NB: The name of the original file is stored in the encrypted file.
     The decripted file is saved with that stored name,
     within the current directory.

The parameters and behavior of this command-line utility are compatible
with those defined for externat script processing by the Code Co-op
distributed source control software.
  ( see <http://www.relisoft.com/co_op/> to learn about Co-op ).
This utility can be used to easily encrypt change scripts that
Co-op automatically transmits by e-mail accross development sites.

Notes:
   - The original file name is stored unencrypted within the encrypted file.
   - A checksum is stored within the encrypted file, so the integrity of the
     transmitted file is checked during decryption.
   - Blowfish is still a relatively safe symmetric encryption algorithm, in widespread use.
     But stronger alternatives exist ( see http://csrc.nist.gov/encryption/aes/rijndael/ ).
   - This utility is designed for simplicity and convenience, not for strong encryption safety.
     It only seeks to provide a simple encryption facility for a small group of developers
     using Code Co-op. (For large networked project, consider a PGP/GnuPG plug-in for
     your email software that will provide authentication and recipient-specific encryption).
   - This code is designed to be written using the standard C++ library only.
     I internally use a version that relies on a platform-abstraction library,
     and relies using memory-mapped files and other tricks
     to simplify code and noticeably improve performance.
     Features that are also missing from the version offered here include random seeding
     of the encrypted message and byte-stealing to avoid padding the end of the message.
   - The code is commented using doxygen tags (see http://www.doxygen.org).
     You may use doxygen to extract documentation from this code in your preferred format.
   - THIS SOFTWARE IS PROVIDED AS IS, WITHOUT ANY GUARANTEE.

Your feedback and comments are welcome !

For all queries/requests/feedback, please use the form
at http://ivan.vecerina.com/contact?subject=coopfish
*/

#include "../../stdafx.h"

#include "blowfish.h"

#include <cstring>
#include <cstdio>
#include <cstdlib>

using namespace std;

namespace coopfish {


/// Rounds up @a value to a multiple of @a rounding.
uint32 roundUp( uint32 value, unsigned rounding )
{
   value += rounding-1;
   value -= value % rounding;
   return value;
}

/// Obtain the byte length of @a file using the standard library.
long fileSize( FILE* file )
{
   long const savedPos = ftell(file);
   fseek( file, 0, SEEK_END );
   long const size = ftell(file);
   fseek( file, savedPos, SEEK_SET );
   return size;
}



static const char* exeName; ///< Name of this executable, obtained from command line
static FILE* src = 00; ///< Source file buffer
static FILE* dst = 00; ///< Destination file buffer

/// Error handling routine. Displays message and usage info, then aborts the application.
void fail( const char* msg )
{
   printf("\n\n"
          " ERROR: %s\n\n"
          " Usage:\n"
          "  To encrypt: %s [key] [EncryptedOutputFile] [InputFile]\n"
          "  To decrypt: %s [key] [EncryptedInputFile]\n"
          " The name of the original file is stored in the encrypted file, and restored accordingly.\n\n"
		 , msg, exeName, exeName );
   if( src ) fclose(src);
   if( dst ) fclose(dst); //NB: also try to delete the destination file in this case ?
   exit(EXIT_FAILURE);
}

/// Write a 4-byte word to the output file in big-endian format
void putWord( uint32 word )
{
   uint8 buf[4] = { uint8(word>>24), uint8(word>>16), uint8(word>>8),  uint8(word) };
   if( ! fwrite( buf, 4, 1, dst ) ) fail("Error writing output file");
}

/// Read a 4-byte word from the source file in big-endian format
uint32 getWord()
{
   uint8 buf[4];
   if( ! fread( buf, 4, 1, src ) ) fail("Error reading input file");
   return (uint32(buf[0])<<24) | (uint32(buf[1])<<16) | (uint32(buf[2])<<8) | buf[3];
}

void putByte( uint8 byte )
{
   if( ! fwrite( &byte, 1, 1, dst ) ) fail("Error writing output file");
}

uint8 getByte()
{
   uint8 ans;
   if( ! fread( &ans, 1, 1, src ) ) fail("Error reading input file");
   return ans;
}


//File header used to identify the file format
static const char magicHead[] = {'c','f','s','h'};



int encrypt(const char* srcName, const char* dstName, const char* const key) {
  int const keyLen = strlen(key);
  if( keyLen<8 || keyLen>56 )
    fail("Key length must be between 8 and 56 characters");

  blowfish::Pad const  pad = blowfish::generatePad( key, keyLen );
  blowfish::Block      chain( 0xC009F158, 0x17EC17EC ); // some dummy initializer -- best would be to use a stored random number instead

  enum { kBufSize = 32*1024 };
  char iobuf[kBufSize];

  src = fopen( srcName, "rb" );
  if( ! src ) fail("Could not open input file");
  dst = fopen( dstName, "wb" );
  if( ! dst ) fail("Could not open output file");
  long const srcSize = fileSize( src );

  { // srcName might be a full/relative path -- we only want to store the file name itself
     const char* scan = srcName;
     while( char const c = *scan++ )
     {
        if( c=='/' || c=='\\' || c==':' ) // take all path delims of unix&windows...
           if( !! *scan ) // only if isn't a trailing char...
              srcName = scan;
     }
  }

  fwrite( magicHead, 4, 1, dst );
  int const srcNameLen = strlen(srcName);
  if( srcNameLen != uint8(srcNameLen) )
     fail("Input file name is too long - limit is 255 chars");
  putByte( srcNameLen );
  fwrite( srcName, srcNameLen, 1, dst );
  putWord( srcSize );

  long sizeLeft = srcSize;
  while( sizeLeft > 0 )
  {
     long sizeRead = fread( iobuf, 1, kBufSize, src );
     sizeLeft -= sizeRead;
     if( sizeLeft == 0 )
        sizeRead = roundUp( sizeRead, blowfish::kBlockSize );
     else if( sizeRead != kBufSize )
        fail("Error while reading input file");
     blowfish::encrypt_CBC( pad, iobuf, iobuf, sizeRead, &chain );
     fwrite( iobuf, 1, sizeRead, dst );
  }

  chain = blowfish::core::encipherBlock( pad, chain );
  putWord( chain.L );
  putWord( chain.R );

  fclose(src); src = 00;
  fclose(dst); dst = 00;
  return EXIT_SUCCESS;
}


int decrypt(const char* srcName, const char* const key) {
  int const keyLen = strlen(key);
  if( keyLen<8 || keyLen>56 )
    fail("Key length must be between 8 and 56 characters");

  blowfish::Pad const  pad = blowfish::generatePad( key, keyLen );
  blowfish::Block      chain( 0xC009F158, 0x17EC17EC ); // some dummy initializer -- best would be to use a stored random number instead

  enum { kBufSize = 32*1024 };
  char iobuf[kBufSize];


  src = fopen( srcName, "rb" );
  if( ! src ) fail("Could not open input file");

  fread( iobuf, 4, 1, src );
  if( strncmp( iobuf, magicHead, 4 ) )
     fail("Incorrect file type was passed as decryption source");
  unsigned const dstNameLen = getByte();
  fread( iobuf, dstNameLen, 1, src );
  iobuf[dstNameLen] = '\0';
  dst = fopen( iobuf, "wb" );
  if( ! dst ) fail("Could not open output file");

  uint32 const dstSize = getWord();
  uint32 const srcSize = roundUp( dstSize, blowfish::kBlockSize );

  long sizeLeft = srcSize;
  while( sizeLeft > 0 )
  {
     long sizeReq = sizeLeft;
     if( sizeReq > kBufSize ) sizeReq = kBufSize;
     long sizeRead = fread( iobuf, 1, sizeReq, src );
     sizeLeft -= sizeRead;
     blowfish::decrypt_CBC( pad, iobuf, iobuf, sizeRead, &chain );
     if( sizeLeft == 0 )
        sizeRead -= ( roundUp( dstSize, blowfish::kBlockSize ) - dstSize );
     fwrite( iobuf, 1, sizeRead, dst );
  }

  chain = blowfish::core::encipherBlock( pad, chain );
  uint32 L = getWord();
  uint32 R = getWord();
  if( chain.L != L  ||  chain.R != R )
     fail("Invalid checksum while decrypting destination file");

  fclose(src); src = 00;
  fclose(dst); dst = 00;
  return EXIT_SUCCESS;
}



//int main(int argc, char** argv)
//{

  //encrypt("d:\\temp\\coopfish\\COPYING.txt", "d:\\temp\\coopfish\\COPYING.txt.cpt", "testtest");

  //decrypt("d:\\temp\\coopfish\\COPYING.txt.cpt", "testtest");



//  return EXIT_SUCCESS;
//#ifndef NDEBUG
//   //void test_blowfish();
//   //try { test_blowfish(); } catch(...) { fail("Failure of internal tests"); }
//#endif
   //exeName = argv[0];
   //switch( argc ) {
   //case  3: printf("Performing decryption of file in \"%s\"\n", argv[2]); break;
   //case  4: printf("Performing encryption of \"%s\" into \"%s\"\n", argv[3], argv[2]); break;
   //default: fail("Invalid Parameters");
   //};
   //const char* const key = argv[1];
   //int const keyLen = strlen(key);
   //if( keyLen<8 || keyLen>56 )
   //   fail("Key length must be between 8 and 56 characters");

   //blowfish::Pad const  pad = blowfish::generatePad( key, keyLen );
   //blowfish::Block      chain( 0xC009F158, 0x17EC17EC ); // some dummy initializer -- best would be to use a stored random number instead

   //enum { kBufSize = 32*1024 };
   //char iobuf[kBufSize];

//   if( argc == 4 ) // encryption
//   {
//      const char*       srcName = argv[3];
//      const char* const dstName = argv[2];
//      src = fopen( srcName, "rb" );
//      if( ! src ) fail("Could not open input file");
//      dst = fopen( dstName, "wb" );
//      if( ! dst ) fail("Could not open output file");
//      long const srcSize = fileSize( src );
//
//      { // srcName might be a full/relative path -- we only want to store the file name itself
//         const char* scan = srcName;
//         while( char const c = *scan++ )
//         {
//            if( c=='/' || c=='\\' || c==':' ) // take all path delims of unix&windows...
//               if( !! *scan ) // only if isn't a trailing char...
//                  srcName = scan;
//         }
//      }
//
//      fwrite( magicHead, 4, 1, dst );
//      int const srcNameLen = strlen(srcName);
//      if( srcNameLen != uint8(srcNameLen) )
//         fail("Input file name is too long - limit is 255 chars");
//      putByte( srcNameLen );
//      fwrite( srcName, srcNameLen, 1, dst );
//      putWord( srcSize );
//
//      long sizeLeft = srcSize;
//      while( sizeLeft > 0 )
//      {
//         long sizeRead = fread( iobuf, 1, kBufSize, src );
//         sizeLeft -= sizeRead;
//         if( sizeLeft == 0 )
//            sizeRead = roundUp( sizeRead, blowfish::kBlockSize );
//         else if( sizeRead != kBufSize )
//            fail("Error while reading input file");
//         blowfish::encrypt_CBC( pad, iobuf, iobuf, sizeRead, &chain );
//         fwrite( iobuf, 1, sizeRead, dst );
//      }
//
//      chain = blowfish::core::encipherBlock( pad, chain );
//      putWord( chain.L );
//      putWord( chain.R );
//   }
//   else // decryption
//   {
//      const char* const srcName = argv[2];
//      src = fopen( srcName, "rb" );
//      if( ! src ) fail("Could not open input file");
//
//      fread( iobuf, 4, 1, src );
//      if( strncmp( iobuf, magicHead, 4 ) )
//         fail("Incorrect file type was passed as decryption source");
//      unsigned const dstNameLen = getByte();
//      fread( iobuf, dstNameLen, 1, src );
//      iobuf[dstNameLen] = '\0';
//      dst = fopen( iobuf, "wb" );
//      if( ! dst ) fail("Could not open output file");
//
//      uint32 const dstSize = getWord();
//      uint32 const srcSize = roundUp( dstSize, blowfish::kBlockSize );
//
//      long sizeLeft = srcSize;
//      while( sizeLeft > 0 )
//      {
//         long sizeReq = sizeLeft;
//         if( sizeReq > kBufSize ) sizeReq = kBufSize;
//         long sizeRead = fread( iobuf, 1, sizeReq, src );
//         sizeLeft -= sizeRead;
//         blowfish::decrypt_CBC( pad, iobuf, iobuf, sizeRead, &chain );
//         if( sizeLeft == 0 )
//            sizeRead -= ( roundUp( dstSize, blowfish::kBlockSize ) - dstSize );
//         fwrite( iobuf, 1, sizeRead, dst );
//      }
//
//      chain = blowfish::core::encipherBlock( pad, chain );
//      uint32 L = getWord();
//      uint32 R = getWord();
//      if( chain.L != L  ||  chain.R != R )
//         fail("Invalid checksum while decrypting destination file");
//   }
//
//   fclose(src); src = 00;
//   fclose(dst); dst = 00;
//   return EXIT_SUCCESS;


//}


};