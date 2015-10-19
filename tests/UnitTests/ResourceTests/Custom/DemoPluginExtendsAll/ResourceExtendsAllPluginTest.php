<?php
/**
 * Smarty PHPUnit tests demo resource plugin extendaall
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for demo resource plugin extendaall tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ResourceExtendsAllPluginTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
     }


    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testResourcePluginExtendsall()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array(
                                          'root' => './templates',
                                          './templates_2',
                                          './templates_3',
                                          './templates_4',
                                      ));

        $expected = "templates\n\n    templates_3\n    templates\n\ntemplates_4";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall.tpl'));
    }

    public function testResourcePluginExtendsallOne()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array(
                                          'root' => './templates',
                                          './templates_2',
                                          './templates_3',
                                          './templates_4',
                                      ));

        $expected = "templates\ntemplates";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall2.tpl'));
    }
}
