/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

// NOTE: Once written, the generated files must be open with a text editor 
// and converted to ANSI (which is the only format that HHC seems to support)
// For example in Notepad++: Menu Format > Convert to ANSI
//
// To open a particular topic (when compiled with chmProcesador):
// hh.exe mk:@MSITStore:c:\full\path\to\Appetizer.chm::1.htm#NameOfAnchor


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


class GenerateHelp: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(GenerateHelp) 


wxString imayle;
wxString gCurrentString;


void WriteHelp(const wxString& filePath, const wxString& htmlString) {
  wxFile file;
  file.Create(filePath, true);
  file.Open(filePath, wxFile::write);

  file.Write(htmlString, wxConvUTF8);
  
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
void A(const wxString& text, const wxString& href, const wxString& name = wxEmptyString) { gCurrentString += _T("<a href='") + Sanitize(href) + _T("'") + _T(" name='") + name + _T("'>") + Sanitize(text) + _T("</a>"); }
void AddAnchor(const wxString& name) { A(_T(""), _T(""), name); }



wxString GenerateHTMLString() {
  gCurrentString = _T("");

  AddStringLn(_T("<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.0 Transitional//EN'>"));
  AddStringLn(_T("<html>"));
  AddStringLn(_T("<head>"));
  // Although the file is going to be written as UTF-8, we set the charset here to
  // Western-1252, which is the only format supported by CHM files. The generated HTML files
  // then need to be converted to ANSI using a text editor.
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
  P(_("Appetizer is a free application launcher, or dock, for Windows. It allows organizing and launching your applications and other shortcuts into a convenient dock. The app is skinable and entirely customizable."));
  P(_("Additionally, the application supports the [http://portableapps.com PortableApps] format. So if you use Appetizer on a removable drive in the PortableApps format, the application will detect it and automatically import all your applications along with the 'Documents', 'Videos', 'Music' and 'Pictures' folders."));
  
  // -------------------------------------------------
  // Features
  // -------------------------------------------------
  H3(_("Features"));

  StartList();
  LI(_("Support for multiple skins"));
  LI(_("Support for plugins"));
  LI(_("Resizable dock, which allows displaying the icons both horizontally or vertically."));
  LI(_("Three sizes of icons: Small, large and extra-large"));
  LI(_("Support for custom icons"));
  LI(_("'Wizard' to automatically imports shortcuts from various locations"));
  LI(_("Organize the icons by drag & dropping them"));
  LI(_("'Multi-launch' functionality"));
  LI(_("Regroup multiple shortcuts within one menu"));
  LI(_("Hot key to hide / show the dock"));
  LI(_("Special items to provide extra functionalities: hide / show the desktop, Recycle Bin, Control Panel, etc."));
  LI(_("Support for multiple languages"));
  LI(_("Minimize to tray icon functionality"));
  EndList();

  IMG(_T("images/Screenshot.jpg"));


  // *******************************************************************************
  // USING APPETIZER
  // *******************************************************************************

  AddAnchor(_T("Usage"));
  H2(_("Using Appetizer"));
  P(_("Appetizer is made to be simple and intuitive. To get more information about an icon or a button, simply move the mouse over it. Additionally, try right-clicking the dock in different places - certain parts of the application such as the icons or the icon panel provide various contextual actions."));
  
  // -------------------------------------------------
  // Adding a shortcut
  // -------------------------------------------------
  H3(_("Adding a shortcut"));
  P(_("To add a shortcut, right-click anywhere on the icon panel and select 'New shortcut', then select a file or folder. Alternatively, you may also directly drag and drop a file onto the dock."));

  // -------------------------------------------------
  // Adding a group
  // -------------------------------------------------  
  H3(_("Adding a group of shortcuts"));
  P(_("To create a group of shortcuts, right-click anywhere on the icon panel, select 'New group', give a name to the group, and click 'Save'. To add shortcuts to this group, click or right-click on it and select 'Organize group shortcuts'. The newly opened window display all your shortcuts in a tree-like control. You can drag & drop these icons to add them to the group. You can also reorder them, still by drag & dropping them. Once this is done, click the 'Close' button."));  

  // -------------------------------------------------
  // Adding a special item
  // -------------------------------------------------
  H3(_("Adding a special items"));
  P(_("Special items provide additional functionalities that would not be possible using a normal shortcut. The special items include a 'Show desktop' button, the Recycle Bin, a link to the Control Panel, etc.")); 
  P(_("To add a special item, right-click on the dock and select 'Add special item'.")); 

  // -------------------------------------------------
  // Modifying or deleting a group or shortcut
  // -------------------------------------------------  
  H3(_("Modifying, moving or deleting a group or shortcut"));
  P(_("To change the properties of an icon, such as its name or location, right-click on it and select 'Properties'. To move an icon to a different location, click and drag it. To remove a shortcut, right-click on it and select 'Remove'.")); 

  // -------------------------------------------------
  // Changing the properties of a shortcut or group
  // -------------------------------------------------  
  AddAnchor(_T("FolderItemProperties"));
  H3(_("Changing the properties of a shortcut or group"));
  P(_("To change the properties of a shortcut or group, right-click on an icon and select 'Properties'. The following options are available:"));  
  StartList();
  LI(_("Name: The name displayed in the tooltip"));
  LI(_("Location: The target of the shortcut. Click on the '...' button to change it."));
  LI(_("Parameters: You may also specify some additional command line arguments in this field. This could be a path to a file or some switches specific to the application. This field is only relevant for executables and is optional."));
  LI(_("Icon: Click on 'Change icon' to select a custom icon. Click on 'Use default' to revert the icon to the default."));
  EndList();

  // -------------------------------------------------
  // Minimizing / Maximizing the dock
  // -------------------------------------------------
  H3(_("Minimizing / Maximizing the dock"));
  P(_("The dock can be minimized to the tray icon by clicking on the 'x' button. To bring it back to front, simply click on the tray icon. Note that this behavior can be changed in the 'Configuration' dialog"));
  P(_("Right-click on the tray icon for additional options."));

  // -------------------------------------------------
  // The option panel
  // -------------------------------------------------
  AddAnchor(_T("OptionPanel"));
  H3(_("The option panel"));
  IMG(_T("images/OptionPanel.jpg"));
  P(_("The option panel pops up when you click on the big arrow button. If you move the mouse over any of its buttons, a tooltip will show up giving you more information."));

  // -------------------------------------------------
  // Importing several shortcuts automatically
  // -------------------------------------------------
  AddAnchor(_T("Import"));
  H3(_("Importing several shortcuts automatically"));
  P(_("It is possible to automatically import shortcuts into Appetizer from various locations, including the Windows 'Start menu', the 'Quick Launch' toolbar, or the PortableApps folder. To do so, click on [images/ImportButton.jpg] in the [#OptionPanel option panel] and select where you would like to import shortcuts from, then click 'Start'."));

  // -------------------------------------------------
  // Launching several application simultaneously
  // -------------------------------------------------
  AddAnchor(_T("Multilaunch"));
  H3(_("Launching several application simultaneously"));
  P(_("Appetizer provides a functionality to launch several applications at the click of a button. This is equivalent to the Startup menu on Windows, except that it launches the app on your portable drive. To use this 'Multi-launch' group, follow these steps:"));
  StartList();
  LI(_("Right-click on any icon and select 'Multi-launch group' from the context menu"));
  LI(_("You can add as many shortcuts as you wish"));
  LI(_("To trigger the 'Multi-launch' group, open the [#OptionPanel option panel] and click on the 'Multi-launch' button: [images/Multilaunch.jpg]"));
  EndList();

  // -------------------------------------------------
  // Adding new plugins
  // -------------------------------------------------
  AddAnchor(_T("AddingPlugins"));
  H3(_("Adding new plugins"));
  P(_("Appetizer supports a plugin system that allows easily adding new functionalities to the dock and customizing it to your needs."));
  //P(_("To add a plugin, "));
  //StartList();
  //LI(_("Right-click on any icon and select 'Multi-launch group' from the context menu"));
  //LI(_("You can add as many shortcuts as you wish"));
  //LI(_("To trigger the 'Multi-launch' group, open the [#OptionPanel option panel] and click on the 'Multi-launch' button: [images/Multilaunch.jpg]"));
  //EndList();

  // *******************************************************************************
  // CONFIGURING APPETIZER
  // *******************************************************************************

  AddAnchor(_T("Configuring"));
  H2(_("Configuring Appetizer"));
  P(_("The configuration dialogue is accessible by clicking on the 'Configuration' button in the [#OptionPanel option panel]. The description of each setting is given below:"));

  AddAnchor(_T("ConfiguringGeneral"));

  // -------------------------
  // General
  // -------------------------
  H3(_("General"));
  StartList();
  LI(_("[b]Language:[/b] Select your language here. If you wish to contribute and translate Appetizer in your own language, please [#Translating follow this link]."));  
  LI(_("[b]Always on top:[/b] If this is selected the bar will always remain on top of the other windows."));
  LI(_("[b]Allow only one instance of Appetizer at a time:[/b] If this is selected, only one instance of Appetizer can run at a time."));
  LI(_("[b]Install autorun file:[/b] Clicking this button will install an autorun file on your removable drive. This will allow Appetizer to automatically start when you insert the drive. However, note that in some instances Windows may prevent the autorun from being executed, for example for security reason or because the computer is set that way."));
  LI(_("[b]Check for update:[/b] Click this button to check if a new update is available."));
  EndList();

  // -------------------------
  // Appearance
  // -------------------------
  AddAnchor(_T("ConfiguringAppearance"));
  H3(_("Appearance"));
  StartList();
  LI(_("[b]Icon size:[/b] Choose the size of the icons."));
  LI(_("[b]Orientation:[/b] The bar can be vertically or horizontally orientated."));
  LI(_("[b]Skin:[/b] Choose one of the available skins."));
  EndList();

  // -------------------------
  // Operations
  // -------------------------
  AddAnchor(_T("ConfiguringOperations"));
  H3(_("Operations"));
  StartList();
  LI(_("[b]Auto-hide after launching an application:[/b] Select this option to have Appetizer hides itself each time you launch an application."));
  LI(_("[b]Run multi-launch group on startup:[/b] Tick this option to automatically run the [#Multilaunch multi-launch group] on startup."));
  LI(_("[b]Hot key to hide / show the bar:[/b] Select the hot key to hide / show the bar. Select 'none' to disable it."));
  EndList();

  // -------------------------
  // Import
  // -------------------------
  AddAnchor(_T("ConfiguringImport"));
  H3(_("Import"));
  StartList();
  LI(_("[b]Exclude these items from the import operations:[/b] Normally, when importing shortcuts using the [#Import import tool], all the executables are added to the dock. If you wish to exclude certain files, add them to this list. Note that this list is also automatically populated whenever you remove a shortcut from the dock."));
  EndList();
	
  // -------------------------
  // Plugins
  // -------------------------
  AddAnchor(_T("ConfiguringPlugins"));
  H3(_("Plugins"));
  // StartList();
  // LI(_("[b]Exclude these items from the import operations:[/b] Normally, when importing shortcuts using the [#Import import tool], all the executables are added to the dock. If you wish to exclude certain files, add them to this list. Note that this list is also automatically populated whenever you remove a shortcut from the dock."));
  // EndList();
	
	

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
  AddAnchor(_T("Translating"));
  H2(_("Translating Appetizer"));
  P(wxString::Format(_("If Appetizer is not available in your own language, you can easily create your own translation. Please [%s follow this link] for more information."), _T("http://app.etizer.org/translations/")));

  // *******************************************************************************
  // MORE LINKS
  // *******************************************************************************
  AddAnchor(_T("Links"));
  H2(_("Support and links"));
  P(wxString::Format(_("Official home page: %s"), _T("[http://app.etizer.org http://app.etizer.org]")));
  P(wxString::Format(_("Project home page: %s"), _T("[https://sourceforge.net/projects/appetizer https://sourceforge.net/projects/appetizer]")));
  P(wxString::Format(_("Beta versions and preleases are usually on [%s PortableApps Beta Testing forum]"), _T("http://portableapps.com/forums/development/beta_testing")));
  P(wxString::Format(_("For bug reports, suggestions, comments or translations, please contact %s"), imayle));//wxString::Format(_T("<a href=\"mailto:%s\">%s</a> or post on the beta forum."), imayle, imayle)));

  AddStringLn(_T("</body>"));
  AddStringLn(_T("</html>"));

  return gCurrentString;
}


bool GenerateHelp::OnInit() {
  InitializeHtmlEntities();

  imayle = _T("tizer@c");
  imayle += _T("ozic.net");
  imayle.Prepend(_T("appe"));

  wxArrayString localeCodes;
  localeCodes.Add(_T("en"));
  localeCodes.Add(_T("fr"));  
  localeCodes.Add(_T("de"));  

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

    htmlString.Replace(imayle, wxString::Format(_T("<a href='mailto:%s'>%s</a>"), imayle, imayle));

    WriteHelp(_T("Data/Help/") + localeCode + _T("/Appetizer.html"), htmlString); 
  }

  return false;
} 