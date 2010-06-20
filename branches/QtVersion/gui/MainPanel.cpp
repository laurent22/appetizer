/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>
#include <FilePaths.h>
#include <MainPanel.h>
#include <Style.h>

using namespace appetizer;


PageData::PageData() {
  tab_ = NULL;
  iconPanel_ = NULL;
  folderItemId_ = -1;
}


PageData::~PageData() {
  SAFE_DELETE(tab_);
  SAFE_DELETE(iconPanel_);
}


FolderItem* PageData::folderItem() {
  return FolderItem::getFolderItemById(folderItemId_);
}


void PageData::setFolderItem(int folderItemId) {
  folderItemId_ = folderItemId;
}



TabSprite* PageData::tab() const {
  return tab_;
}


IconPanel* PageData::iconPanel() const {
  return iconPanel_;
}


void PageData::setTab(TabSprite* tab) {
  tab_ = tab;
}


void PageData::setIconPanel(IconPanel* iconPanel) {
  iconPanel_ = iconPanel;
}


MainPanel::MainPanel(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  rootFolderItemId_ = -1;
  pageIndex_ = -1;

  QString backgroundFile = FilePaths::GetSkinFile("Background.png");

  backgroundSprite_ = new NineSliceItem(this->parentWindow());
  backgroundSprite_->loadBackgroundImage(backgroundFile);
  addItem(backgroundSprite_);

  scrollPane_ = new ScrollPane(this->parentWindow());
  addItem(scrollPane_);

  lastDrawnMaskSize_ = QSize(0, 0);

  maskNineSlicePainter_.loadImage(backgroundFile);

  QObject::connect(UserSettings::instance(), SIGNAL(settingChanged(UserSetting*)), this, SLOT(userSettings_settingChanged(UserSetting*)));
}


MainPanel::~MainPanel() {
  clearPageData_();
}


void MainPanel::userSettings_settingChanged(UserSetting* setting) {
  if (setting->name() == "IconSize") {
    for (int i = 0; i < (int)pages_.size(); i++) {
      PageData* page = pages_[i];
      if (page->iconPanel() && page->folderItem()) {
        page->iconPanel()->setIconSize(page->folderItem()->displayIconSize());
      }
    }
    invalidate();
  }
}


void MainPanel::clearPageData_() {
  for (int i = 0; i < (int)pages_.size(); i++) {
    PageData* page = pages_.at(i);
    SAFE_DELETE(page);
  }
  pages_.clear();
}


PageData* MainPanel::page() {
  if (pageIndex_ < 0) return NULL;
  return getPage(pageIndex_);
}


FolderItem* MainPanel::rootFolderItem() {
  return FolderItem::getFolderItemById(rootFolderItemId_);
}


void MainPanel::loadFolderItems(int rootFolderItemId) {
  clearPageData_();

  rootFolderItemId_ = rootFolderItemId;

  FolderItem* rootFolderItem = this->rootFolderItem();
  if (!rootFolderItem) {
    qCritical() << "This folder item doesn't exist:" << rootFolderItemId;
    return;
  }

  for (int i = 0; i < rootFolderItem->numChildren(); i++) {
    FolderItem* sectionFolderItem = rootFolderItem->getChildAt(i);
    
    if (sectionFolderItem->type() != FolderItem::Type_Section) {
      qCritical() << "Folder item structure is wrong - section expected:" << rootFolderItemId;
      return;
    }

    TabSprite* tab = new TabSprite(this->parentWindow());
    addItem(tab);
    connect(tab, SIGNAL(mouseReleased()), this, SLOT(tab_clicked()));
    tab->loadFolderItem(sectionFolderItem->id());

    PageData* page = new PageData();
    page->setTab(tab);
    page->setFolderItem(sectionFolderItem->id());
    pages_.push_back(page);
  }

  pageIndex_ = 0;
  showPage(0);
  invalidate();
}


void MainPanel::tab_clicked() {
  TabSprite* sender = static_cast<TabSprite*>(QObject::sender());
  
  int pageIndex = -1;
  for (int i = 0; i < (int)pages_.size(); i++) {
    PageData* d = pages_[i];
    if (d->tab() == sender) {
      pageIndex = i;
      break;
    }
  }

  if (pageIndex < 0) {
    qCritical() << "No page associated with this tab";
    return;
  }

  showPage(pageIndex);
}


PageData* MainPanel::getPage(int index) {
  return pages_[index];
}


PageData* MainPanel::showPage(int index) {
  PageData* page = getPage(index);
  pageIndex_ = index;

  if (!page->iconPanel()) {
    FolderItem* folderItem = page->folderItem();
    IconPanel* iconPanel = new IconPanel(this->parentWindow());
    iconPanel->loadFolderItems(folderItem->id());
    iconPanel->setIconSize(folderItem->displayIconSize());
    page->setIconPanel(iconPanel);
  }

  for (int i = 0; i < (int)pages_.size(); i++) {
    PageData* page = pages_[i];
    IconPanel* iconPanel = page->iconPanel();
    if (!iconPanel || !iconPanel->parentItem()) continue;
    ((GraphicsItem*)(iconPanel->parentItem()))->removeItem(iconPanel);
  }

  addItem(page->iconPanel());

  if (page) {
    IconPanel* iconPanel = page->iconPanel();
    scrollPane_->setContent(iconPanel);
  } else {
    scrollPane_->setContent(NULL);
  }

  invalidate();

  return page;
}


NineSliceItem* MainPanel::backgroundSprite() const {
  return backgroundSprite_;
}


void MainPanel::updateDisplay() {
  GraphicsItem::updateDisplay();

  backgroundSprite_->resize(width(), height());

  int tabX = Style::background.padding.left + Style::tab.margin.left;
  int tabY = Style::background.padding.top + Style::tab.margin.top;
  int panelY = tabY;
  int tabWidth = 0;
  if (pages_.size() > 0) {
    tabWidth = (width() - Style::background.padding.width - Style::tab.margin.width * pages_.size() + Style::tab.margin.right) / pages_.size();
  }
  
  for (int i = 0; i < (int)(pages_.size()); i++) {
    PageData* page = pages_[i];
    TabSprite* tab = page->tab();
    tab->move(tabX, tabY);
    tab->setWidth(tabWidth);

    tabX += tab->width() + Style::tab.margin.right;
    panelY = tab->y() + tab->height();
  }

  panelY = panelY + Style::iconPanel.padding.top;

  int scrollPaneWidth = width() - Style::background.padding.width;
  int scrollPaneHeight = height() - panelY - Style::background.padding.bottom;
  scrollPane_->setX(Style::background.padding.left);
  scrollPane_->setY(panelY);
  scrollPane_->resize(scrollPaneWidth, scrollPaneHeight);
  scrollPane_->setVisible(scrollPaneWidth > 0 && scrollPaneHeight > 0);
}