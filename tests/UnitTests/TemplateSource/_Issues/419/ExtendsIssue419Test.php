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
 */
class ExtendsIssue419Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testextends419()
    {
        $this->smarty->left_delimiter = '{{';
        $this->smarty->right_delimiter = '}}';
        $this->assertEquals('child', $this->smarty->fetch('extends:001_parent.tpl|001_child.tpl'));
    }

}
