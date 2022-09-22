<?php
/**
 * Smarty PHPunit tests compiler errors
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 *
 * Short syntax of  section properties did not work
 */
class SectionPropertiesShortSyntaxIssue428Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testSection_001()
    {
        $this->smarty->assign('foo', array('a', 'b', 'c'));
        $this->assertEquals('abc', $this->smarty->fetch('001_section.tpl'));
    }
    public function testSection_002()
    {
        $this->smarty->assign('foo', array('a', 'b', 'c'));
        $this->assertEquals('abc', $this->smarty->fetch('002_section.tpl'));
    }


}
