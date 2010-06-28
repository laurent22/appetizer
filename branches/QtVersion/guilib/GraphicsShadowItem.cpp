/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <GraphicsShadowItem.h>
#include <ImagingUtil.h>

using namespace appetizer;


GraphicsShadowItem::GraphicsShadowItem(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  source_ = NULL;
  pixmap_ = NULL;
  updatePixmap_ = true;

  blurRadius_ = 5;
  color_ = QColor(0,0,0,125);
  xOffset_ = 2;
  yOffset_ = 2;
}


GraphicsShadowItem::~GraphicsShadowItem() {
  SAFE_DELETE(pixmap_);
}


void GraphicsShadowItem::setBlurRadius(int v) {
  if (blurRadius_ == v) return;
  blurRadius_ = v;
  updatePixmap_ = true;
  invalidate();
}


void GraphicsShadowItem::setXOffset(int v) {
  if (xOffset_ == v) return;
  xOffset_ = v;
  invalidate();
}


void GraphicsShadowItem::setYOffset(int v) {
  if (yOffset_ == v) return;
  yOffset_ = v;
  invalidate();
}


void GraphicsShadowItem::setColor(const QColor& v) {
  if (color_ == v) return;
  color_ = v;
  updatePixmap_ = true;
  invalidate();
}


void GraphicsShadowItem::setSource(GraphicsItem* v) {
  if (source_) {
    QObject::disconnect(source_, SIGNAL(resized()), this, SLOT(source_resized()));
    QObject::disconnect(source_, SIGNAL(moved()), this, SLOT(source_moved()));
  }
  
  source_ = v;

  QObject::connect(source_, SIGNAL(resized()), this, SLOT(source_resized()));
  QObject::connect(source_, SIGNAL(moved()), this, SLOT(source_moved()));

  updatePixmap_ = true;
  invalidate();
}


void GraphicsShadowItem::source_resized() {
  updatePixmap_ = true;
  invalidate();
}


void GraphicsShadowItem::source_moved() {
  updatePixmap_ = true;
  invalidate();
}


void GraphicsShadowItem::updateDisplay() {
  GraphicsItem::updateDisplay();

  SAFE_DELETE(pixmap_);

  if (source_ && source_->parentItem() && source_->isVisible()) {
    stackBefore(source_);
    move(source_->x() - blurRadius() + xOffset(), source_->y() - blurRadius() + yOffset());
    setVisible(true);
  } else {
    setVisible(false);
  }

  if (updatePixmap_ && source_) {
    QImage image(source_->width(), source_->height(), QImage::Format_ARGB32_Premultiplied);
    image.fill(qRgba(0,0,0,0));
    QPainter p(&image);
    QStyleOptionGraphicsItem options;
    source_->paint(&p, &options, NULL);
    p.end();

    image = ImagingUtil::fillKeepAlpha(image, color());
    image = ImagingUtil::blur(image, blurRadius());

    pixmap_ = new QPixmap(QPixmap::fromImage(image));

    updatePixmap_ = true;
  }
}


void GraphicsShadowItem::paint(QPainter *painter, const QStyleOptionGraphicsItem* option, QWidget* widget) {
  GraphicsItem::paint(painter, option, widget);

  if (pixmap_) painter->drawPixmap(QPoint(0,0), *pixmap_);
}