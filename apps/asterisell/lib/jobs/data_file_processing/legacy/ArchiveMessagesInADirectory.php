<?php

/* $LICENSE 2013:
 *
 * Copyright (C) 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Archive files in a directory, without any additional service.
 */
class ArchiveMessagesInADirectory extends FileMessageQueueProcessor
{

    const QUEUE_NAME = 'simple_message_archive_input';

    public function getArchiveDirectory()
    {
        return getAsterisellCompleteRootDirectory() . '/data_files/simple_message_archive/' . FileMessageQueue::INPUT_DIR_NAME;
    }

    public function getInputQueueName()
    {
        return self::QUEUE_NAME;
    }

    public function getOutputQueueName()
    {
        return 'simple_message_archive';
    }

    protected function processCurrentMessage(PDO $conn)
    {
        $this->getInputQueue()->moveCurrentFileToQueue($this->getOutputQueue(), null);
    }
}
