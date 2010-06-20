/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#pragma warning(disable: 4345) // warning C4345: behavior change: an object of POD type constructed with an initializer of the form () will be default-initialized
#pragma warning(disable: 4996) // warning C4996: 'std::basic_string<_Elem,_Traits,_Ax>::copy': Function call with parameters that may be unsafe - this call relies on the caller to check that the passed values are correct. To disable this warning, use -D_SCL_SECURE_NO_WARNINGS. See documentation on how to use Visual C++ 'Checked Iterators'

#include <ctype.h>
#include <fstream>
#include <sys/stat.h>

#include "MoParser.h"


using namespace std;


const int32_t GettextMoParser::HEADER_MAGIC_NUMBER    = 0x950412de;
const int32_t GettextMoParser::HEADER_MAGIC_NUMBER_SW = 0xde120495;


GettextMoParser::GettextMoParser() {
	swappedBytes_ = false;
  moFileHeader_ = NULL;
  moData_ = NULL;
  charset_ = NULL;
  charsetParsed_ = false;
  ready_ = false;
}


int32_t GettextMoParser::swap_(int32_t ui) const {
  return swappedBytes_ ? (ui << 24) | ((ui & 0xff00) << 8) |
                         ((ui >> 8) & 0xff00) | (ui >> 24)
                         : ui;
}


void GettextMoParser::clearData() {
  if (moData_) delete moData_;
  if (charset_) delete charset_;

  swappedBytes_ = false;
  moFileHeader_ = NULL;
  moData_ = NULL;
  charset_ = NULL;
  charsetParsed_ = false;

  for (int i = 0; i < (int)messages_.size(); i++) {
    TranslatedMessage* message = messages_.at(i);
    if (message->original) {
      delete[] message->original->string;
      delete message->original;
    }
    if (message->translated) {
      delete[] message->translated->string;
      delete message->translated;
    }
    delete message;
  }
  messages_.clear();

  ready_ = false;
}


bool GettextMoParser::parseFile(const char* filePath) {
  clearData();
  
  struct stat fileInfo;

  if (stat(filePath, &fileInfo) != 0) {
    // Cannot get file size
    clearData();
    return false;
  }

  char* moData = new char[fileInfo.st_size];

  ifstream moFile(filePath, ios::in | ios::binary);
  if (!moFile.read(moData, fileInfo.st_size)) {
    // Cannot read file data
    clearData();
    return false;
  }

  return parse(moData);
}


bool GettextMoParser::parse(char* moData) {
  moData_ = moData;

  moFileHeader_ = (MoFileHeader*)moData_;
  if (moFileHeader_->magic != HEADER_MAGIC_NUMBER && moFileHeader_->magic != HEADER_MAGIC_NUMBER_SW) {
    // Invalid header
    clearData();
    return false;
  }

  // If the magic number bytes are swapped, all the other numbers will have
  // to be swapped
  swappedBytes_ = moFileHeader_->magic == HEADER_MAGIC_NUMBER_SW;

  moFileHeader_->magic = swap_(moFileHeader_->magic);
  moFileHeader_->revision = swap_(moFileHeader_->revision);
  moFileHeader_->numStrings = swap_(moFileHeader_->numStrings);
  moFileHeader_->offsetOriginalStrings = swap_(moFileHeader_->offsetOriginalStrings);
  moFileHeader_->offsetTranslatedStrings = swap_(moFileHeader_->offsetTranslatedStrings);
  moFileHeader_->hashTableSize = swap_(moFileHeader_->hashTableSize);
  moFileHeader_->offsetHashTable = swap_(moFileHeader_->offsetHashTable);

  ready_ = true;

  return true;
}


GettextMoParser::~GettextMoParser() {
  clearData();
}


char* GettextMoParser::charset() const {
  if (charset_ || charsetParsed_) return charset_;
  if (!moData_) return NULL;  

  charsetParsed_ = true;

  MoOffsetTableItem* translationTable = (MoOffsetTableItem*)(moData_ + moFileHeader_->offsetTranslatedStrings);
  translationTable->length = swap_(translationTable->length);
  translationTable->offset = swap_(translationTable->offset);

  char* infoBuffer = (char*)(moData_ + translationTable->offset);
  std::string info(infoBuffer);
  size_t contentTypePos = info.find("Content-Type: text/plain; charset=");
  if (contentTypePos == info.npos) return NULL;
  
  size_t stringStart = contentTypePos + 34; // strlen("Content-Type: text/plain; charset=")
  size_t stringEnd = info.find('\n', stringStart);
  if (stringEnd == info.npos) return NULL;
  
  int charsetLength = stringEnd - stringStart;
  if (charsetLength == 0) return NULL;

  charset_ = new char[charsetLength + 1];
  info.copy(charset_, charsetLength, stringStart);
  
  charset_[charsetLength] = '\0';

  if (strcmp(charset_, "CHARSET") == 0) {
    delete[] charset_;
    charset_ = NULL;
  }

  // To lowercase
  for(int i = 0; i < (int)strlen(charset_); ++i) charset_[i] = tolower(charset_[i]);

  return charset_;
}


GettextMessage* GettextMoParser::getTranslation(const char* originalString, int originalLength) {

  // Check if the string has already been looked up, in which case it is in the messages_
  // vector. If found, return it and exit now.

  for (int i = 0; i < (int)messages_.size(); i++) {
    TranslatedMessage* message = messages_.at(i);
    if (strcmp(originalString, message->original->string) == 0) {
      return message->translated;
    }
  }

  // Look for the original message

  MoOffsetTableItem* originalTable = (MoOffsetTableItem*)(moData_ + moFileHeader_->offsetOriginalStrings);
  originalTable->length = swap_(originalTable->length);
  originalTable->offset = swap_(originalTable->offset);
  
  int stringIndex;
  bool found = false;

  for (stringIndex = 0; stringIndex < moFileHeader_->numStrings; stringIndex++) {
    char* currentString = (char*)(moData_ + originalTable->offset);

    if (strcmp(currentString, originalString) == 0) {
      found = true;
      break;
    }
    
    originalTable++;
    originalTable->length = swap_(originalTable->length);
    originalTable->offset = swap_(originalTable->offset);
  }


  TranslatedMessage* message = new TranslatedMessage();
  

  char* originalStringCopy = new char[originalLength + 1];
  strcpy(originalStringCopy, originalString);
  originalStringCopy[originalLength] = '\0';

  GettextMessage* mOriginal = new GettextMessage();
  mOriginal->length = originalTable->length;
  mOriginal->string = originalStringCopy;

  message->original = mOriginal;

  if (!found) {
    // Couldn't find orignal string
    messages_.push_back(message);
    message->translated = NULL;
    return NULL;
  }

  // At this point, stringIndex is the index of the message and originalTable points to
  // the original message data.

  // Get the translated string and build and the output message structure

  MoOffsetTableItem* translationTable = (MoOffsetTableItem*)(moData_ + moFileHeader_->offsetTranslatedStrings);
  translationTable += stringIndex;
  translationTable->length = swap_(translationTable->length);
  translationTable->offset = swap_(translationTable->offset);

  char* translatedString = new char[translationTable->length + 1];
  char* tempString = (char*)(moData_ + translationTable->offset);

  strncpy(translatedString, tempString, translationTable->length);
  translatedString[translationTable->length] = '\0';

  
  GettextMessage* mTranslated = new GettextMessage();
  mTranslated->length = translationTable->length;
  mTranslated->string = translatedString;
  
  message->translated = mTranslated;

  messages_.push_back(message);

  return message->translated;
}