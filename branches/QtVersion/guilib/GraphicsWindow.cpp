/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <GraphicsWindow.h>

using namespace appetizer;

GraphicsWindow::GraphicsWindow(): QWidget(NULL, Qt::FramelessWindowHint | Qt::WindowSystemMenuHint) {
  frameRate_ = 30;
  lastUpdateTime_ = -9999;
  timeBetweenFrames_ = -1;
  time_.start();

  setWindowFlags(Qt::FramelessWindowHint | Qt::Widget);
  SetWindowLong(winId(), GWL_EXSTYLE, GetWindowLong(winId(), GWL_EXSTYLE) | WS_EX_LAYERED);	

  updateTimer_.setSingleShot(true);
  QObject::connect(&updateTimer_, SIGNAL(timeout()), this, SLOT(updateTimer_timeout()));
}


int GraphicsWindow::timeBetweenFrames() const {
  if (timeBetweenFrames_ <= 0) {
    timeBetweenFrames_ = (int)(1000.0 / (float)frameRate_);
  }
  return timeBetweenFrames_;
}


QPixmap* GraphicsWindow::getWindowPixmap() {
	qCritical() << "GraphicsWindow::getWindowPixmap must be implemented";
	return NULL;
}


void GraphicsWindow::updateTimer_timeout() {
  QPixmap* pixmap = getWindowPixmap();
  if (pixmap) updateDisplay(*pixmap);
}


void GraphicsWindow::invalidateDisplay() {
  int deltaTime = time_.elapsed() - lastUpdateTime_;

  if (deltaTime >= timeBetweenFrames_) {
    updateTimer_.setInterval(1);
    updateTimer_.start();
  } else {
    if (!updateTimer_.isActive()) {
      updateTimer_.setInterval(timeBetweenFrames_ - deltaTime);
      updateTimer_.start();
    }
  }
}


void GraphicsWindow::updateDisplay(QPixmap& pixmap) {
  HBITMAP oldBitmap;
  HBITMAP hBitmap;	
  SIZE size;
  size.cx = pixmap.width();
  size.cy = pixmap.height();
  HDC screenDc = GetDC(NULL);
  POINT pointSource;
  pointSource.x = 0;
  pointSource.y = 0; 
  POINT topPos;
  topPos.x = x();
  topPos.y = y();	
  HDC memDc = CreateCompatibleDC(screenDc);
  BLENDFUNCTION blend;
  blend.BlendOp             = AC_SRC_OVER;
  blend.BlendFlags          = 0;
  blend.SourceConstantAlpha = 255;
  blend.AlphaFormat         = AC_SRC_ALPHA;
  hBitmap = pixmap.toWinHBITMAP(QPixmap::PremultipliedAlpha); 
  oldBitmap = (HBITMAP)SelectObject(memDc, hBitmap);
  UpdateLayeredWindow(winId(), screenDc,  &topPos,  &size, memDc,  &pointSource, 0, &blend, ULW_ALPHA);
  ReleaseDC(NULL, screenDc);
  if (hBitmap != NULL) {
    SelectObject(memDc, oldBitmap);
    DeleteObject(hBitmap); 
    DeleteObject(hBitmap);
  }
  DeleteDC(memDc); 

  lastUpdateTime_ = time_.elapsed();
}