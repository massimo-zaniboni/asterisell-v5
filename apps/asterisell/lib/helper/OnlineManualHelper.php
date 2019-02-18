<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * @param $section NULL for home page of manual
 * @param $name the name to display to user
 * @return a HTML link to a section of online manual
 */
function link_to_online_manual($section, $name) {
   $r =  '<a href="' . _compute_public_path('index', 'help', 'html', true);
   if (!is_null($section)) {
       $r .= '#' . $section;
   }
   $r .=  '" target="_blank">' . $name . '</a>';

   return $r;
}

?>