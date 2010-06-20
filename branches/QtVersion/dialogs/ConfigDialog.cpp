/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <ConfigDialog.h>

using namespace appetizer;


ConfigDialog::ConfigDialog(QWidget* parent): QDialog(parent) {
  
  
  topLayout_ = new QBoxLayout(QBoxLayout::Up, this);
  setLayout(topLayout_);

  tabWidget_ = new QTabWidget(this);
  topLayout_->addWidget(tabWidget_);

  std::vector<QString> labels = UserSettings::instance()->getGroupLabels();

  for (int i = 0; i < (int)labels.size(); i++) {
    QString label = labels[i];
    QWidget* test1 = new QWidget(this);
    tabWidget_->addTab(test1, label);
  }

}


ConfigDialog::~ConfigDialog() {

}