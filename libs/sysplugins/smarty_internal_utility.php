<?php
/**
 * Project:     Smarty: the PHP compiling template engine
 * File:        smarty_internal_utility.php
 * SVN:         $Id: $
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * For questions, help, comments, discussion, etc., please join the
 * Smarty mailing list. Send a blank e-mail to
 * smarty-discussion-subscribe@googlegroups.com
 *
 * @link       http://www.smarty.net/
 * @copyright  2008 New Digital Group, Inc.
 * @author     Monte Ohrt <monte at ohrt dot com>
 * @author     Uwe Tews
 * @package    Smarty
 * @subpackage PluginsInternal
 * @version    3-SVN$Rev: 3286 $
 */

/**
 * Utility class
 *
 * @package    Smarty
 * @subpackage Security
 */
class Smarty_Internal_Utility
{
    /**
     * private constructor to prevent calls creation of new instances
     */
    final private function __construct()
    {
        // intentionally left blank
    }

    /**
     * Return array of tag/attributes of all tags used by an template
     *
     * @param Smarty_Internal_Template $template
     *
     * @throws Exception
     * @throws SmartyException
     * @return array                    of tag/attributes
     */
    public static function getTags(Smarty_Internal_Template $template)
    {
        $template->smarty->get_used_tags = true;
        $template->compileTemplateSource();

        return $template->used_tags;
    }
}
