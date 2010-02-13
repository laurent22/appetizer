-- Copyright (C) 2008 Laurent Cozic. All right reserved.
-- Use of this source code is governed by a GNU/GPL license that can be
-- found in the LICENSE file.


-- *****************************************************************
--
-- Register preferences
--
-- *****************************************************************

preferences:registerPreference({
	type = "CheckBox",
	name = "showTrayIcon",
	defaultValue = "1",
	title = _("Show tray icon")
})

function applyPluginPreferences()
	appetizer:showTrayIcon(preferences:getValue("showTrayIcon"))
end

function appetizer_ready()
	applyPluginPreferences()
end

function plugin_preferenceChange()
	applyPluginPreferences()
end

-- Register a global event handler. "appetizer_ready()" is going
-- to be called when the application is ready
appetizer:addEventListener("ready", "appetizer_ready")
plugin:addEventListener("preferenceChange", "plugin_preferenceChange")