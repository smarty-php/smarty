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
    public function setUp()
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
     * test standard xml Smarty::PHP_QUOTE
     */
    public function testXmlPhpQuote()
    {
        $this->smarty->compile_id = 'PHP_QUOTE';
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test standard xml Smarty::PHP_ALLOW
     */
    public function testXmlPhpAllow()
    {
        $this->smarty->compile_id = 'PHP_ALLOW';
        $this->smarty->security_policy->php_handling = Smarty::PHP_ALLOW;
        $tpl = $this->smarty->createTemplate('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $this->smarty->fetch($tpl));
    }

    /**
     * test xml caching
     */
    public function testXmlCaching()
    {
        $this->smarty->security_policy->php_handling = Smarty::PHP_PASSTHRU;
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
        $this->smarty->security_policy->php_handling = Smarty::PHP_PASSTHRU;
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml_main.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }

    /**
     * test xml caching PhpQuote
     */
    public function testXmlCachingPhpQuote()
    {
        $this->smarty->compile_id = 'PHP_QUOTE';
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml.tpl');
        $this->assertEquals('<?xml version="1.0" encoding="UTF-8"?>', $content);
    }

    /**
     * test xml caching PhpAllow
     */
    public function testXmlCachingPhpAllow()
    {
        $this->smarty->compile_id = 'PHP_ALLOW';
        $this->smarty->security_policy->php_handling = Smarty::PHP_ALLOW;
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $content = $this->smarty->fetch('xml.tpl');
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
