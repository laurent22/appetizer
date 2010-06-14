/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Gettext_MoParser_H
#define Gettext_MoParser_H


#ifdef _MSC_VER
typedef __int32 int32_t;
typedef unsigned __int32 uint32_t;
typedef __int64 int64_t;
typedef unsigned __int32 uint64_t;
#else
#include <stdint.h>
#endif


struct GettextMessage {
  char* string;
  int length;
};

struct TranslatedMessage {
  GettextMessage* original;
  GettextMessage* translated;
};

typedef std::vector<TranslatedMessage*> TranslatedMessages;


class GettextMoParser {

public:

  GettextMoParser();
  ~GettextMoParser();
  bool parseFile(const char* filePath);
  bool parse(char* moData);
  void clearData();
  GettextMessage* getTranslation(const char* originalString, int originalLength); 
  char* charset() const;
  inline bool ready() const { return ready_; }

  static const int32_t GettextMoParser::HEADER_MAGIC_NUMBER;
  static const int32_t GettextMoParser::HEADER_MAGIC_NUMBER_SW;

private:

  struct MoFileHeader {
    int32_t magic;                    // offset +00:  magic id
    int32_t revision;                 //        +04:  revision
    int32_t numStrings;               //        +08:  number of strings in the file
    int32_t offsetOriginalStrings;    //        +0C:  start of original string table
    int32_t offsetTranslatedStrings;  //        +10:  start of translated string table
    int32_t hashTableSize;            //        +14:  hash table size
    int32_t offsetHashTable;          //        +18:  offset of hash table start
  };

  struct MoOffsetTableItem {
    int32_t length;    // length of the string
    int32_t offset;    // pointer to the string
  };

  int32_t swap_(int32_t ui) const;
  bool swappedBytes_;

  MoFileHeader* moFileHeader_;
  char* moData_;
  TranslatedMessages messages_;
  mutable char* charset_;
  mutable bool charsetParsed_;
  bool ready_;

};

#endif // Gettext_MoParser_H
