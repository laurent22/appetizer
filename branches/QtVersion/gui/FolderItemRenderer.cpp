/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Application.h>
#include <FolderItemRenderer.h>
#include <Style.h>
#include <Constants.h>
using namespace appetizer;


FolderItemRenderer::FolderItemRenderer() {
  folderItemId_ = -1;
  iconSize_ = -1;

  setIconSize(SMALL_ICON_SIZE);
}


void FolderItemRenderer::setIconSize(int size) {
  if (iconSize_ == size) return;
  iconSize_ = size;
  setWidth(size + Style::icon.padding.width);
  setHeight(size + Style::icon.padding.height);
  invalidate();
}


int FolderItemRenderer::iconSize() const {
  return iconSize_;
}


void FolderItemRenderer::setFolderItem(int folderItemId) {
  folderItemId_ = folderItemId;
}


FolderItem* FolderItemRenderer::folderItem() const {
  return FolderItem::getFolderItemById(folderItemId_);
}


void FolderItemRenderer::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);  

  FolderItem* f = folderItem();
  if (!f) return;

  QPixmap* icon = f->getIconPixmap(Application::instance()->getNextValidIconSize(iconSize()));
  if (!icon) return;

  painter->setRenderHint(QPainter::SmoothPixmapTransform);
  painter->drawPixmap(
    QRect(Style::icon.padding.left, Style::icon.padding.top, width() - Style::icon.padding.width, height() - Style::icon.padding.height),
    *icon);
}