/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include <wx/cmdline.h>
#include <wx/intl.h>
#include <wx/arrstr.h>
#include <wx/file.h>
#include <wx/regex.h>
#include "HtmlEntities.h"

extern wxArrayString heSymbols;
extern wxArrayString heCode;


// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class GenerateHelp: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(GenerateHelp) 


wxString gCurrentString;


void WriteHelp(const wxString& filePath, const wxString& htmlString) {
  wxFile file;
  file.Create(filePath, true);
  file.Open(filePath, wxFile::write);
  file.Write(htmlString, wxConvISO8859_1);
  file.Close();
}


void AddStringLn(const wxString& s) {
  gCurrentString += s + _T("\n");
}


wxString Sanitize(const wxString& text) {
  wxString output;
  for (int i = 0; i < text.Len(); i++) {
    wxString c1 = text.Mid(i, 1);
    
    bool done = false;
    for (int j = 0; j < heSymbols.Count(); j++) {
      wxString c2 = heSymbols[j];

      if (c1 == c2) {
        output += heCodes[j];
        done = true;
        break;
      }
    }

    if (!done) output += c1;

  }

  return output;
}


void H1(const wxString& text) { gCurrentString += _T("\n<h1>") + Sanitize(text) + _T("</h1>\n"); }
void H2(const wxString& text) { gCurrentString += _T("\n<h2>") + Sanitize(text) + _T("</h2>\n"); }
void H3(const wxString& text) { gCurrentString += _T("\n<h3>") + Sanitize(text) + _T("</h3>\n"); }
void H4(const wxString& text) { gCurrentString += _T("\n<h4>") + Sanitize(text) + _T("</h4>\n"); }
void P(const wxString& text) { gCurrentString += _T("<p>") + Sanitize(text) + _T("</p>\n"); }
void IMG(const wxString& src) { gCurrentString += _T("<img src='") + Sanitize(src) + _T("'/>\n"); }
void BR() { gCurrentString += _T("<br/>\n"); }
void StartList() { gCurrentString += _T("<ul>\n"); } 
void EndList() { gCurrentString += _T("</ul>\n"); } 
void LI(const wxString& text) { gCurrentString += _T("<li>") + Sanitize(text) + _T("</li>\n"); }
void A(const wxString& text, const wxString& href, const wxString& name) { gCurrentString += _T("<a href='") + Sanitize(href) + _T("'") + _T(" name='") + name + _T("'>") + Sanitize(text) + _T("</a>"); }


wxString GenerateHTMLString() {
  gCurrentString = _T("");

  AddStringLn(_T("<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.0 Transitional//EN'>"));
  AddStringLn(_T("<html>"));
  AddStringLn(_T("<head>"));
  AddStringLn(_T("<meta http-equiv='content-type' content='text/html; charset=Western-1252'>"));
  AddStringLn(_T("<title>Appetizer</title>"));

  AddStringLn(_T("<style TYPE='text/css'>"));
	AddStringLn(_T("<!--"));
  AddStringLn(_T("h2 { color:#ff4f02; }"));
  AddStringLn(_T("h3 {  }"));
	AddStringLn(_T("-->"));
	AddStringLn(_T("</style>"));

  AddStringLn(_T("</head>"));
  AddStringLn(_T("<body>"));

  IMG(_T("images/Logo.jpg"));  

  // *******************************************************************************
  // INTRODUCTION
  // *******************************************************************************

  H2(_("Introduction"));
  H3(_("About Appetizer"));
  P(_("Appetizer is a dock bar that allows you to organize, display and launch your applications and other shortcuts. Currently it is possible to add executables, documents and folders to the dock. Additionally, if you use Appetizer on a USB key in the [http://portableapps.com PortableApps] format, the application will detect it and automatically add all your applications along with the 'Documents', 'Videos', 'Music' and 'Pictures' folders."));
  
  // -------------------------------------------------
  // Features
  // -------------------------------------------------
  H3(_("Features"));

  StartList();
  LI(_("Resizable dock, which allows displaying the icons both horizontally or vertically."));
  LI(_("Support for multiple skins"));
  LI(_("Two size of icons: Small and large"));
  LI(_("Organize the icons by drag & dropping them"));
  LI(_("'Multi-launch' functionality"));
  LI(_("Regroup multiple shortcuts within one menu"));
  LI(_("Support for multiple languages"));
  LI(_("Minimize to tray icon functionality"));
  EndList();

  IMG(_T("images/Screenshot.jpg"));


  // *******************************************************************************
  // USING APPETIZER
  // *******************************************************************************

  H2(_("Using Appetizer"));
  P(_("Appetizer is made to be simple and intuitive. To get more information about an icon or a button, simply move the mouse over it. Additionally, try right-clicking the dock in different places - certain parts of the application such as the icons or the icon panel provide various contextual actions."));
  
  // -------------------------------------------------
  // Adding a shortcut
  // -------------------------------------------------
  H3(_("Adding a shortcut"));
  P(_("To add a shortcut, right-click anywhere on the icon panel and select 'New shortcut', then click on [images/SelectFile.jpg] to select a file or [images/SelectFolder.jpg] to select a folder. Once this is done, the 'Name' field should be automatically populated based on the file or folder you've chosen. Finally, you may also specify some additional parameters in the 'Parameters' field. This could be a path to a file or some switches specific to the application. This field is only relevant for executables and is optional."));  

  // -------------------------------------------------
  // Adding a group
  // -------------------------------------------------  
  H3(_("Adding a group of shortcuts"));
  P(_("To create a group of shortcuts, right-click anywhere on the icon panel, select 'New group', give a name to the group, and click 'Save'. To add shortcuts to this group, click or right-click on it and select 'Organize group shortcuts'. The newly opened window display all your shortcuts in a tree-like control. You can drag & drop these icons to add them to the group. You can also reorder them in a different, still by drag & dropping them. Once this is done, click the 'Close' button."));  

  // -------------------------------------------------
  // Changing the icon of a group
  // -------------------------------------------------  
  H3(_("Changing the icon of a group"));
  P(_("A group can take the icon of any of the shortcuts it contains. To do so, right-click on a group and select 'Properties'. Then click on 'Select a different icon'. If the group doesn't contain any shortcut, only the default icon can be selected."));  

  // -------------------------------------------------
  // Modifying or deleting a group or shortcut
  // -------------------------------------------------  
  H3(_("Modifying, moving or deleting a group or shortcut"));
  P(_("To change the properties of an icon, such as its name or location, right-click on it and select 'Properties'. To move an icon to a different location, click and drag it. To remove a shortcut, right-click on it and select 'Remove'.")); 

  // -------------------------------------------------
  // Minimizing / Maximizing the dock
  // -------------------------------------------------
  H3(_("Minimizing / Maximizing the dock"));
  P(_("The dock can be minimized to the tray icon by clicking on the 'x' button. To bring it back to front, simply click on the tray icon."));
  P(_("Right-click on the tray icon for additional options."));

  // -------------------------------------------------
  // The option panel
  // -------------------------------------------------
  A(_T(""), _T(""), _T("OptionPanel"));
  H3(_("The option panel"));
  IMG(_T("images/OptionPanel.jpg"));
  P(_("The option panel pops up when you click on the big arrow button. If you move the mouse over any of its buttons, a tooltip will show up giving you more information."));

  // -------------------------------------------------
  // Launching several application simultaneously
  // -------------------------------------------------
  H3(_("Launching several application simultaneously"));
  P(_("Appetizer provides a functionality to launch several applications at the click of a button. This is equivalent to the Startup menu on Windows, except that it launches the app on your portable drive. To use this 'Multi-launch' group, follow these steps:"));
  StartList();
  LI(_("Right-click on any icon and select 'Multi-launch group' from the context menu"));
  LI(_("You can add as many shortcuts as you wish"));
  LI(_("To trigger the 'Multi-launch' group, open the [#OptionPanel option panel] and click on the 'Multi-launch' button: [images/Multilaunch.jpg]"));
  EndList();

  // *******************************************************************************
  // CONFIGURING APPETIZER
  // *******************************************************************************
  H2(_("Configuring Appetizer"));
  P(_("The configuration dialogue is accessible by clicking on the 'Configuration' button in the [#OptionPanel option panel]. The description of each setting is given below:"));

  H3(_("General"));
  StartList();
  LI(_("[b]Language:[/b] Select your language here. If you wish to contribute and translate Appetizer in your own language, please [#Translating follow this link]."));
  LI(_("[b]Auto-hide after launching an application:[/b] Select this option to have Appetizer hides itself each time you launch an application."));
  LI(_("[b]Always on top:[/b] If this is selected the bar will always remain on top of the other windows."));
  LI(_("[b]Allow only one instance of Appetizer at a time:[/b] If this is selected, only one instance of Appetizer can run at a time."));
  LI(_("[b]Check for update:[/b] Click this button to check if a new update is available."));
  EndList();

  H3(_("Appearance"));
  StartList();
  LI(_("[b]Icon size:[/b] Choose the size of the icons, as displayed on the bar."));
  LI(_("[b]Orientation:[/b] The bar can be vertically or horizontally orientated."));
  LI(_("[b]Skin:[/b] Choose one of the available skins."));
  EndList();

  // *******************************************************************************
  // OTHER TIPS
  // *******************************************************************************
  H2(_("Other tips"));
  StartList();
  LI(_("You can resize the dock by clicking and dragging its bottom right corner."));
  LI(_("You can move it by clicking anywhere on the icon panel and dragging."));
  LI(_("The bar will snap to the borders of the screens as you move it."));
  EndList();

  // *******************************************************************************
  // TRANSLATING APPETIZER
  // *******************************************************************************
  A(_T(""), _T(""), _T("Translating"));
  H2(_("Translating Appetizer"));
  P(_("If Appetizer is not available in your own language, you can easily create your own translation. To do so, please follow these steps:"));
  StartList();
  LI(_("[http://www.poedit.net/ Download Poedit] and install it. This is a utility that makes it easier to create and maintain translation files."));
  LI(_("Open the folder where you've installed Appetizer and open the Data\\Locale folder"));
  LI(_("Copy and paste the 'en' folder."));
  LI(_("Within that folder, open 'appetizer.po' with Poedit."));
  LI(_("In Poedit, go into the Catalog menu and click Configuration. Change 'Country' and 'Language' to your own country and language."));
  LI(_("Now, to translate a line of text, click on it and add your translated text in the bottom text field."));
  LI(_("Once all the text is translated, please email 'appetizer.po' and 'appetizer.mo' to appetizer@cozic.net"));
  EndList();
  P(_("Your translation will be added in the next release."));

  H3(_("Translating the help file"));
  P(_("If you wish to translate the help file, follow the same steps as above and edit the file in Data\\Help\\en\\appetizer.po."));

  AddStringLn(_T("</body>"));
  AddStringLn(_T("</html>"));

  return gCurrentString;
}


bool GenerateHelp::OnInit() {
  InitializeHtmlEntities();

  wxArrayString localeCodes;
  localeCodes.Add(_T("en"));
  localeCodes.Add(_T("fr"));  

  for (int i = 0; i < localeCodes.Count(); i++) {
    wxString localeCode = localeCodes[i];
    const wxLanguageInfo* info = wxLocale::FindLanguageInfo(localeCode);
    if (!info) {
      wxLogDebug(_T("CANNOT GET LANGUAGE INFO"));
      continue;
    }

    wxLocale locale;
    locale.Init(info->Language);
    locale.AddCatalogLookupPathPrefix(_T("Data/Help"));
    locale.AddCatalog(_T("appetizer_help"));

    wxString htmlString = GenerateHTMLString();

    wxRegEx imageRegEx(_T("\\[([^\\s]+(\\.png|\\.gif|\\.jpg))\\]"), wxRE_ADVANCED);
    wxRegEx urlRegEx(_T("\\[((ftp|http|https)://[^\\s]+)\\s([^\\]]+)\\]"), wxRE_ADVANCED);    
    wxRegEx strongRegEx(_T("\\[b\\](.*?)\\[\\/b\\]"), wxRE_ADVANCED);
    wxRegEx internalUrlRegEx(_T("\\[(#[^\\s]+)\\s([^\\]]+)\\]"), wxRE_ADVANCED);

    imageRegEx.ReplaceAll(&htmlString, _T("<img src='\\1'/>"));
    urlRegEx.ReplaceAll(&htmlString, _T("<a href='\\1'>\\3</a>"));
    strongRegEx.ReplaceAll(&htmlString, _T("<b>\\1</b>"));
    internalUrlRegEx.ReplaceAll(&htmlString, _T("<a href='\\1'>\\2</a>"));

    WriteHelp(_T("Data/Help/") + localeCode + _T("/Appetizer.html"), htmlString); 
  }

  return false;
} 