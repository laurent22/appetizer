/** @file
   Implementation of the BLOWFISH algorithm.
   Reference:  <http://www.counterpane.com/blowfish.html>
   @author     Ivan Vecerina - 2002 - This source file is hereby placed in the public domain
*/
#ifndef _included_BLOWFISH_H_
#define _included_BLOWFISH_H_

// quick and dirty guesses... to be fixed based on your platform.
typedef unsigned char uint8;  // an 8-bit unsigned byte
typedef unsigned long uint32; // 4-byte unsigned integer


namespace blowfish {

   /// Unit of encryption. Block of data processed together during an encryption cycle.
   struct Block
   {
	   Block(uint32 l, uint32 r) : L(l), R(r) {}
	   uint32 L, R; ///< Two data words being shuffled together.
   };

   /// Source and destination data sizes must be multiples of this value
   enum { kBlockSize = 2*4 }; ///< two 4-byte words.

   /// Data pad used for encryption. Pseudo-random block used to have varying encryption results.
   struct Pad
   {
	   uint32   P[18];        ///< P-box for 16 round encryption
	   uint32   S[4][256];    ///< S-box for byte value shuffeling
   };


   /// Returns an encription pad generated using the specified key/password.
   Pad   generatePad( void const* keyPtr, int keyLen );



   /**
   @par Encryption Modes
      - @b ECB : Direct encryption of independent blocks (no data chaining).
      - @b CBC : Encrypt after xor-ing source with previous encrypted result.
      - @b CFB : Destination is source xor-ed with encrypted previous result.
   */

   void encrypt_ECB( Pad const& pad, void const* src, void* dst, size_t byteSize );
   void decrypt_ECB( Pad const& pad, void const* src, void* dst, size_t byteSize );

   void encrypt_CBC( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain=00 );
   void decrypt_CBC( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain=00 );

   void encrypt_CFB( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain=00 );
   void decrypt_CFB( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain=00 );



   /// Low-level utilities - for advanced users only
   namespace core {

      extern Pad const defaultPiPad;   ///< default pad based on the decimals of pi

      // Block encryption primitives
      Block encipherBlock( Pad const& pad, Block const& block);
      Block decipherBlock( Pad const& pad, Block const& block);
   }//namespace internal



}//namespace blowfish



#endif // _included_BLOWFISH_H_

