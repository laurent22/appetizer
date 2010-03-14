/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <azMainWindow.h>

azMainWindow::azMainWindow() {
  scene_ = new azMainScene();

  scene_->addText("Hello, world!");

  setScene(scene_);
}


void azMainWindow::resizeEvent(QResizeEvent* event) {
  QGraphicsView::resizeEvent(event);

  scene_->setSceneRect(0, 0, event->size().width(), event->size().height());
  scene_->invalidate();
}