/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <NineSlicePainter.h>
using namespace appetizer;

NineSlicePainter::NineSlicePainter() {
    image_ = NULL;
    gridIsExplicitelySet_ = false;
}

bool NineSlicePainter::isNull() {
  return !image_;
}


QImage* NineSlicePainter::image() const {
  return image_;
}


void NineSlicePainter::loadImage(QImage* image) {
    SAFE_DELETE(image_);
    image_ = image;
}

void NineSlicePainter::loadImage(QString filePath) {
    SAFE_DELETE(image_)
    image_ = new QImage(filePath);
}

void NineSlicePainter::setGrid(int x, int y, int width, int height) {
    grid_.setLeft(x);
    grid_.setTop(y);
    grid_.setWidth(width);
    grid_.setHeight(height);
    gridIsExplicitelySet_ = true;
}

void NineSlicePainter::drawImage(QPainter* painter, int x, int y, int width, int height) {
    if (width == 0 || height == 0) return;

    QImage* workImage = image_;

    if (width == workImage->width() && height == workImage->height()) {
      // Optimization: if the source bitmap has the same size as the
      // destination size, we blit it directly.
      painter->drawImage(QPoint(x, y), *workImage);
      return;
    }

    int destX, destY, destWidth, destHeight;
    int sourceX, sourceY, sourceWidth, sourceHeight;

    QRect grid;

    if (gridIsExplicitelySet_) {
        grid = QRect(grid_.left(), grid_.top(), grid_.width(), grid_.height());
    } else {
        if (image_->width() <= width) {
          grid.setLeft(floor((double)(image_->width() / 3)));
        } else {
          grid.setLeft(floor((double)(width / 3)));
        }

        grid.setWidth(image_->width() - grid.left() * 2);

        if (image_->height() <= height) {
          grid.setTop(floor((double)(image_->height() / 3)));
        } else {
          grid.setTop(floor((double)(height / 3)));
        }

        grid.setHeight(image_->height() - grid.top() * 2);

        grid_ = grid;
    }

    int rightWidth = workImage->width() - grid.right();
    int bottomHeight = workImage->height() - grid.bottom();

    for (int i = 0; i < 9; i++) {

      switch(i) {

        case 0: case 3: case 6:

          destX = 0;
          destWidth = grid.left();
          sourceX = 0;
          sourceWidth = grid.left();
          break;

        case 1: case 4: case 7:

          destX = grid.left();
          destWidth = width - destX - rightWidth;
          sourceX = grid.left();
          sourceWidth = grid.width();
          break;

        case 2: case 5: case 8:

          destX = width - rightWidth;
          destWidth = rightWidth;
          sourceX = grid.right();
          sourceWidth = workImage->width() - grid.right();
          break;

      } // switch

      switch(i) {

        case 0:

          destY = 0;
          destHeight = grid.top();
          sourceY = 0;
          sourceHeight = grid.top();
          break;

        case 3:

          destY = grid.top();
          destHeight = height - destY - bottomHeight;
          sourceY = grid.top();
          sourceHeight = grid.height();
          break;

        case 6:

          destY = height - bottomHeight;
          destHeight = bottomHeight;
          sourceY = grid.bottom();
          sourceHeight = workImage->height() - grid.bottom();
          break;

      } // switch

      painter->drawImage(QRect(destX, destY, destWidth, destHeight), *workImage, QRect(sourceX, sourceY, sourceWidth, sourceHeight));

    } // for

}


NineSlicePainter::~NineSlicePainter() {
  if (image_) {
    delete image_;
    image_ = NULL;
  }
}
