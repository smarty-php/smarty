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
class ModifierIssue327Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->registerPlugin('modifier', 'substr', 'substr');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testModifier327()
    {
          $this->assertEquals('hello you', $this->smarty->fetch('string:{"hello world"|substr:0:-5|cat:"you"}'));
    }

}
