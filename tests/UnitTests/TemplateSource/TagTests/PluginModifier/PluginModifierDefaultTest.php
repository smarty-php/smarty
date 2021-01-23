<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierDefaultTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{$array.a.b|default:$array.c:\'defaultval\'}');

        $this->smarty->assign('array', []);
        $this->assertEquals('defaultval', $this->smarty->fetch($tpl));
    }
}
