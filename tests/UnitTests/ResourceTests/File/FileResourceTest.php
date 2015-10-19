<?php
/**
 * Smarty PHPunit tests for File resources
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for file resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class FileResourceTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function relative($path)
    {
        $path = str_replace(str_replace("\\", "/", dirname(__FILE__)), '.', str_replace("\\", "/", $path));

        return $path;
    }

    /**
     *
     */
    public function testGetTemplateFilepath()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals($this->normalizePath("./templates/helloworld.tpl"), $tpl->source->filepath);
    }

    public function testTemplateFileExists1()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->source->exists);
    }

    public function testTemplateFileExists2()
    {
        $this->assertTrue($this->smarty->templateExists('helloworld.tpl'));
    }

    public function testTemplateFileNotExists1()
    {
        $tpl = $this->smarty->createTemplate('notthere.tpl');
        $this->assertFalse($tpl->source->exists);
    }

    public function testTemplateFileNotExists2()
    {
        $this->assertFalse($this->smarty->templateExists('notthere.tpl'));
    }

    public function testTemplateFileNotExists3()
    {
        try {
            $result = $this->smarty->fetch('notthere.tpl');
        }
        catch (Exception $e) {
            $this->assertContains('Unable to load template file \'notthere.tpl\'', $e->getMessage());

            return;
        }
        $this->fail('Exception for not existing template is missing');
    }

    public function testGetTemplateTimestamp()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->source->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->source->getTimeStamp()));
    }

    public function testGetTemplateSource()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals('hello world', $tpl->source->getContent());
    }

    public function testUsesCompiler()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertFalse($tpl->source->handler->uncompiled);
    }

    public function testIsEvaluated()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertFalse($tpl->source->handler->recompiled);
    }

    public function testGetCompiledFilepath()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'helloworld.tpl', 'file', $this->smarty->getTemplateDir(0))
            , $tpl->compiled->filepath
        );
    }

    public function testGetCompiledTimestampPrepare()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        // create dummy compiled file
        file_put_contents($tpl->compiled->filepath, '<?php ?>');
        touch($tpl->compiled->filepath, $tpl->source->getTimeStamp());
    }

    /**
     *
     */
    public function testGetCompiledTimestamp()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->compiled->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->compiled->getTimeStamp()));
        $this->assertEquals($tpl->compiled->getTimeStamp(), $tpl->source->getTimeStamp());
    }

    public function testMustCompileExisting()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertFalse($tpl->mustCompile());
    }

    public function testMustCompileAtForceCompile()
    {
        $this->smarty->setForceCompile(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->mustCompile());
    }

    public function testMustCompileTouchedSourcePrepare()
    {
        // touch to prepare next test
        sleep(2);
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        touch($tpl->source->filepath);
    }
        public function testMustCompileTouchedSource()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->mustCompile());
        // clean up for next tests
        $this->cleanDir($this->smarty->getCompileDir());
    }

    public function testCompileTemplateFile()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->compileTemplateSource();
        $this->assertTrue(file_exists($tpl->compiled->filepath));
    }

    public function testGetCachedFilepath()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals($this->buildCachedPath($tpl, false, null, null, 'helloworld.tpl', 'file', $this->smarty->getTemplateDir(0), 'file')
            , $tpl->cached->filepath
        );
    }

    public function testGetCachedTimestamp()
    {
        // create dummy cache file for the following test
        file_put_contents($this->buildCachedPath($this->smarty, false, null, null, 'helloworld.tpl', 'file', $this->smarty->getTemplateDir(0), 'file')
            , '<?php ?>');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->cached->timestamp));
        $this->assertEquals(10, strlen($tpl->cached->timestamp));
    }


    public function testGetRenderedTemplate()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals('hello world', $tpl->fetch());
    }

    public function testRelativeInclude()
    {
        $result = $this->smarty->fetch('relative.tpl');
        $this->assertContains('hello world', $result);
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRelativeIncludeSub()
    {
        $result = $this->smarty->fetch('sub/relative.tpl');
        $this->assertContains('hello world', $result);
    }

    public function testRelativeIncludeFail()
    {
        try {
            $this->smarty->fetch('relative_sub.tpl');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("Unable to load template"), $e->getMessage());

            return;
        }
        $this->fail('Exception for unknown relative filepath has not been raised.');
    }

    public function testRelativeIncludeFailOtherDir()
    {
        $this->smarty->addTemplateDir('./templates_2');
        try {
            $this->smarty->fetch('relative_notexist.tpl');
        }
        catch (Exception $e) {
            $this->assertContains("Unable to load template", $e->getMessage());

            return;
        }
        $this->fail('Exception for unknown relative filepath has not been raised.');
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRelativeFetch()
    {
        $this->smarty->setTemplateDir(array(
                                          dirname(__FILE__) . '/does-not-exist/',
                                          dirname(__FILE__) . '/templates/sub/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('./relative.tpl'));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRelativeFetch2()
    {
         $this->smarty->setTemplateDir(array(
                                          dirname(__FILE__) . '/does-not-exist/',
                                          dirname(__FILE__) . '/templates/sub/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('../helloworld.tpl'));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRelativeFetchCwd()
    {
        $cwd = getcwd();
        chdir(dirname(__FILE__) . '/templates/sub/');
        $dn = dirname(__FILE__);
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          dirname(__FILE__) . '/does-not-exist/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('./relative.tpl'));
        chdir($cwd);
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRelativeFetchCwd2()
    {
        $cwd = getcwd();
        chdir(dirname(__FILE__) . '/templates/sub/');
        $dn = dirname(__FILE__);
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          dirname(__FILE__) . '/does-not-exist/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('../helloworld.tpl'));
        chdir($cwd);
    }

    protected function _relativeMap($map, $cwd = null)
    {
        foreach ($map as $file => $result) {
            $this->cleanDir($this->smarty->getCompileDir());
            $this->cleanDir($this->smarty->getCacheDir());

            if ($result === null) {
                try {
                    $this->smarty->fetch($file);
                    if ($cwd !== null) {
                        chdir($cwd);
                    }

                    $this->fail('Exception expected for ' . $file);

                    return;
                }
                catch (SmartyException $e) {
                    // this was expected to fail
                }
            } else {
                try {
                    $_res = $this->smarty->fetch($file);
                    $this->assertEquals(str_replace("\r", '', $result), $_res, $file);
                }
                catch (Exception $e) {
                    if ($cwd !== null) {
                        chdir($cwd);
                    }

                    throw $e;
                }
            }
        }

        if ($cwd !== null) {
            chdir($cwd);
        }
    }

    public function testRelativity()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = dirname(__FILE__);
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/',
                                      ));

        $map = array(
            'foo.tpl'                               => 'theory',
            './foo.tpl'                             => 'theory',
            '././foo.tpl'                           => 'theory',
            '../foo.tpl'                            => 'relativity',
            '.././foo.tpl'                          => 'relativity',
            './../foo.tpl'                          => 'relativity',
            'einstein/foo.tpl'                      => 'einstein',
            './einstein/foo.tpl'                    => 'einstein',
            '../theory/einstein/foo.tpl'            => 'einstein',
            'templates/relativity/relativity.tpl'   => 'relativity',
            './templates/relativity/relativity.tpl' => 'relativity',
        );

        $this->_relativeMap($map);

        $this->smarty->setTemplateDir(array(
                                          './templates/relativity/theory/',
                                      ));

        $map = array(
            'foo.tpl'                               => 'theory',
            './foo.tpl'                             => 'theory',
            '././foo.tpl'                           => 'theory',
            '../foo.tpl'                            => 'relativity',
            '.././foo.tpl'                          => 'relativity',
            './../foo.tpl'                          => 'relativity',
            'einstein/foo.tpl'                      => 'einstein',
            './einstein/foo.tpl'                    => 'einstein',
            '../theory/einstein/foo.tpl'            => 'einstein',
            'templates/relativity/relativity.tpl'   => 'relativity',
            './templates/relativity/relativity.tpl' => 'relativity',
        );

        $this->_relativeMap($map);
    }

    public function testRelativityCwd()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = dirname(__FILE__);

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/',
                                      ));

        $map = array(
            'foo.tpl'                    => 'theory',
            './foo.tpl'                  => 'theory',
            '././foo.tpl'                => 'theory',
            '../foo.tpl'                 => 'relativity',
            '.././foo.tpl'               => 'relativity',
            './../foo.tpl'               => 'relativity',
            'einstein/foo.tpl'           => 'einstein',
            './einstein/foo.tpl'         => 'einstein',
            '../theory/einstein/foo.tpl' => 'einstein',
        );

        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityPrecedence()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = dirname(__FILE__);

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/einstein/',
                                      ));

        $map = array(
            'foo.tpl'       => 'einstein',
            './foo.tpl'     => 'einstein',
            '././foo.tpl'   => 'einstein',
            '../foo.tpl'    => 'theory',
            '.././foo.tpl'  => 'theory',
            './../foo.tpl'  => 'theory',
            '../../foo.tpl' => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/einstein/',
                                      ));
        $this->_relativeMap($map, $cwd);

        $map = array(
            '../theory.tpl'           => 'theory',
            './theory.tpl'            => 'theory',
            '../../relativity.tpl'    => 'relativity',
            '../relativity.tpl'       => 'relativity',
            './einstein.tpl'          => 'einstein',
            'einstein/einstein.tpl'   => 'einstein',
            './einstein/einstein.tpl' => 'einstein',
        );

        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/einstein/',
                                      ));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityRelRel()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = dirname(__FILE__);

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          '../..',
                                      ));

        $map = array(
            'foo.tpl'     => 'relativity',
            './foo.tpl'   => 'relativity',
            '././foo.tpl' => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/einstein');
        $this->smarty->setTemplateDir(array(
                                          '../..',
                                      ));
        $this->_relativeMap($map, $cwd);

        $map = array(
            'relativity.tpl'      => 'relativity',
            './relativity.tpl'    => 'relativity',
            'theory/theory.tpl'   => 'theory',
            './theory/theory.tpl' => 'theory',
        );

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array(
                                          '../..',
                                      ));
        $this->_relativeMap($map, $cwd);

        $map = array(
            'foo.tpl'                         => 'theory',
            './foo.tpl'                       => 'theory',
            'theory.tpl'                      => 'theory',
            './theory.tpl'                    => 'theory',
            'einstein/einstein.tpl'           => 'einstein',
            './einstein/einstein.tpl'         => 'einstein',
            '../theory/einstein/einstein.tpl' => 'einstein',
            '../relativity.tpl'               => 'relativity',
            './../relativity.tpl'             => 'relativity',
            '.././relativity.tpl'             => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityRelRel1()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = dirname(__FILE__);
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));

        $map = array(
            'foo.tpl'                         => 'theory',
            './foo.tpl'                       => 'theory',
            'theory.tpl'                      => 'theory',
            './theory.tpl'                    => 'theory',
            'einstein/einstein.tpl'           => 'einstein',
            './einstein/einstein.tpl'         => 'einstein',
            '../theory/einstein/einstein.tpl' => 'einstein',
            '../relativity.tpl'               => 'relativity',
            './../relativity.tpl'             => 'relativity',
            '.././relativity.tpl'             => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));
        $this->_relativeMap($map, $cwd);
    }
}
