<?php
/**
 * Smarty PHPunit tests  of the <?xml...> tag handling
 *

 * @author  Uwe Tews
 */

/**
 * class for <?xml...> tests
 *
 * 
 * 
 * 
 */
class XmlTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
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
    public function testXml($dummy = null)
    {
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test standard xml
     */
    public function testXmlPhpAllow($dummy = null)
    {
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test xml caching
     */
    public function testXmlCaching($dummy = null)
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }
    /**
     * test subtemplate xml caching
     */
    public function testXmlCaching2($dummy = null)
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml_main.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }


    /**
     * test xml with variable
     */
    public function testXmlVariable($dummy = null)
    {
        $this->smarty->assign('foo','bar');
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="bar"?>', $content);
    }
    /**
     * test xml with nocache variable
     */
    public function testXmlVariableNocache1($dummy = null)
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('foo','bar',true);
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="bar"?>', $content);
    }
    public function testXmlVariableNocache2($dummy = null)
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('foo','foo',true);
        $content = $this->smarty->fetch('xmlvar.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="foo"?>', $content);
    }
}
