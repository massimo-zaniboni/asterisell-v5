#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Massimo Zaniboni'
SITENAME = u'Asterisell'
SITESUBTITLE = u'Open Source Application for Rating and Showing VoIP Calls'

PATH = 'content'
STATIC_PATHS = ['images']

TIMEZONE = 'Europe/Rome'

DEFAULT_LANG = u'en'

THEME='Flex'
SITELOGO = '/images/asterisell_logo2.png'
FAVICON='/images/asterisell_favicon.ico'

COPYRIGHT_YEAR = 2018
COPYRIGHT_NAME='Massimo Zaniboni'
HOME_HIDE_TAGS=True
ARTICLE_HIDE_TRANSLATION=True

# Feed generation is usually not desired when developing
FEED_ATOM = None
FEED_ALL_ATOM = None

FEED_DOMAIN = 'https://www.asterisell.com'
FEED_RSS='index.rss'
FEED_MAX_ITEMS=20

CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

DEFAULT_CATEGORY = 'new release'
MAIN_MENU = False

SOCIAL = (('github', 'https://github.com/massimo-zaniboni/asterisell-v5'),
          ('rss', 'https://www.asterisell.com/index.rss'))

MENUITEMS = (('Archives', '/archives.html'),
             ('Categories', '/categories.html'),
             ('Tags', '/tags.html'),)

LINKS = (('Admin Demo', 'https://demo.asterisell.com/admin'),
         ('Customer Demo', 'https://demo.asterisell.com'),
         ('Documentation', '/manual/manual.html'),
         ('News','/'))

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True
