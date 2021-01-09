<?php
/**
 * Smarty PHPunit tests  of the <?xml...> tag handling
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for <?xml...> tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class XmlTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->enableSecurity();
        $this->smarty->setForceCompile(true);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test standard xml
     */
    public function testXml()
    {
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test standard xml
     */
    public function testXmlPhpAllow()
    {
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test xml caching
     */
    public function testXmlCaching()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }
    /**
     * test subtemplate xml caching
     */
    public function testXmlCaching2()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml_main.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }


    /**
     * test xml with variable
     */
    public function testXmlVariable()
    {
        $this->smarty->assign('foo','bar');
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="bar"?>', $content);
    }
    /**
     * test xml with nocache variable
     */
    public function testXmlVariableNocache1()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('foo','bar',true);
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="bar"?>', $content);
    }
    public function testXmlVariableNocache2()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('foo','foo',true);
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="foo"?>', $content);
    }
}
