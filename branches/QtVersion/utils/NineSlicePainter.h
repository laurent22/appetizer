/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef NINESLICEPAINTER_H
#define NINESLICEPAINTER_H
namespace appetizer {

class NineSlicePainter {

public:

    NineSlicePainter();
    void loadImage(QImage* image);
    void loadImage(QString filePath);
    void setGrid(int x, int y, int width, int height);
    void drawImage(QPainter* painter, int x, int y, int width, int height);
    bool isNull();
    ~NineSlicePainter();

private:

    QImage* image_;
    QRect grid_;
    bool gridIsExplicitelySet_;

};

}
#endif // NINESLICEPAINTER_H
