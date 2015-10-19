<?php
/**
 * Smarty PHPunit tests compilation of strip tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for strip tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileStripTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test strip tag
     */
    public function testStrip()
    {
        $tpl = $this->smarty->createTemplate("eval:{strip}<table>\n </table>{/strip}");
        $this->assertEquals('<table></table>', $this->smarty->fetch($tpl));
    }
}
