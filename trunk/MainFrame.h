/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __MainFrame_H
#define __MainFrame_H


#include "imaging/NineSlicesPainter.h"
#include "bitmap_controls/ImagePanel.h"
#include "bitmap_controls/NineSlicesPanel.h"
#include "bitmap_controls/ImageButton.h"
#include "IconPanel.h"
#include "TypeDefinitions.h"
#include "OptionPanel.h"
#include "MiniLaunchBar.h"
#include "ApplicationTrayIcon.h"
#include "gui/AboutDialog.h"


enum {
  ID_BUTTON_Arrow,
  ID_TIMER_OpenCloseAnimation
};


class MainFrame: public wxFrame {

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      bool Resizing;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
      wxSize InitWindowSize;
    };
    
    bool rotated_;
    bool needLayoutUpdate_;
    bool needMaskUpdate_;
    bool optionPanelOpen_;
    AboutDialog* aboutDialog_;
    NineSlicesPanel* backgroundPanel_;
    IconPanel* iconPanel_;
    NineSlicesPainter maskNineSlices_;
    WindowDragDataStruct windowDragData_;
    ImagePanel* resizerPanel_;
    wxLogWindow* logWindow_;
    ImageButton* arrowButton_;
    wxBitmap* arrowButtonCloseIcon_;
    wxBitmap* arrowButtonOpenIcon_;
    OptionPanel* optionPanel_;    
    int optionPanelOpenWidth_;
    int optionPanelMaxOpenWidth_;
    long openCloseAnimationStartTime_;
    long openCloseAnimationDuration_;
    bool openCloseAnimationDockLeft_;
    int openCloseAnimationWindowRight_;
    wxIcon frameIcon_;
    ApplicationTrayIcon taskBarIcon_;
    bool firstIdleEventSent_;
    ImageButton* closeSideButton_;
    ImageButton* ejectSideButton_;
    ImageButton* minimizeSideButton_;
    bool hotKeyRegistered_;
    bool activated_;
    wxPanel* nullPanel_;

    void UpdateMask();
    void UpdateLayout();
    void UpdateLayout(int width, int height);

  public:

    MainFrame();
    ~MainFrame();

    IconPanel* GetIconPanel();
    OptionPanel* GetOptionPanel();
    wxPanel* GetNullPanel();

    void DoAutoHide();

    int GetMinHeight();
    int GetMinWidth();
    int GetMaxHeight();
    int GetMaxWidth();

    void ConvertToWindowValidCoordinates(const wxDisplay* display, int& x, int& y, int& width, int& height);

    wxMenu* GetContextMenu();

    int GetOptionPanelTotalWidth();

    void Localize();

    bool IsOptionPanelOpen() const { return optionPanelOpen_; }

    void OpenOptionPanel(bool open = true);
    void CloseOptionPanel();
    void ToggleOptionPanel();
    void InvalidateLayout();
    void InvalidateMask();
    void ApplySkin(const wxString& skinName = wxEmptyString);

    void UnregisterHideShowHotKey();
    bool RegisterHideShowHotKey();

    void SetRotated(bool rotated, bool swapWidthAndHeight = false);

    int GetDisplayIndex();
    bool IsLeftOfDisplay();
    bool IsTopOfDisplay();

    void RecurseCleanUp(wxWindow* window);

    void OnPaint(wxPaintEvent& evt);
    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseMove(wxMouseEvent& evt);
    void OnResizerMouseDown(wxMouseEvent& evt);
    void OnResizerMouseUp(wxMouseEvent& evt);
    void OnResizerMouseMove(wxMouseEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnMove(wxMoveEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);  
    void OnClose(wxCloseEvent& evt);
    void OnImageButtonClick(wxCommandEvent& evt);
    void OnIdle(wxIdleEvent& evt);
    void OnMouseCaptureLost(wxMouseCaptureLostEvent& evt);
    void OnHotKey(wxKeyEvent& evt);
    void OnActivate(wxActivateEvent& evt);

  DECLARE_EVENT_TABLE()

};

#endif