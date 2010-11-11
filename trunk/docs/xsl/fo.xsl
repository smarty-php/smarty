<?xml version='1.0'?>
<!-- 

  fo.xsl: FO specific customizations (as a base for pdf)

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0">

<xsl:import href="docbook/fo/docbook.xsl"/>
<xsl:include href="common.xsl"/>


<!-- Enable FOP extensions -->
<xsl:param name="fop.extensions" select="1"/>

<!-- Adjust the paper-Type -->
<xsl:param name="paper.type">
 <xsl:choose>
  <xsl:when test="$l10n.gentext.language='de' or
                  $l10n.gentext.language='hu'">'A4'</xsl:when>
  <xsl:otherwise>'USletter'</xsl:otherwise>
 </xsl:choose>
</xsl:param>

<!-- Don't move TITLEs to the left -->
<xsl:param name="title.margin.left" select="'-0pc'"/>


<!-- Let's give the terms in varlistentries enough space -->
<xsl:param name="variablelist.as.blocks" select="1"/>


<!-- ==========================   TOC   =================================== -->

<xsl:param name="toc.section.depth">1</xsl:param>

<!-- Supress the ",figure,example,equation" -->
<xsl:param name="generate.toc">
/appendix toc,title
article/appendix  nop
/article  toc,title
book      toc,title
chapter   toc,title
part      toc,title
/preface  toc,title
qandadiv  toc
qandaset  toc
reference toc,title
/sect1    toc
/sect2    toc
/sect3    toc
/sect4    toc
/sect5    toc
/section  toc
set       toc,title
</xsl:param>


<!-- ======================  PAGENUMBERS IN TOC  ========================= -->
<!-- Give the following elements an id, so that the toc can point to them  -->
<!-- Exception PART: Here the TOC in the part itself is added by hand -->

<!-- Correct page numbers for PARTS in main-TOC
     and provide an own TOC in the several parts -->
<xsl:template match="part">
  <xsl:if test="not(partintro)">
    <xsl:variable name="id">
      <xsl:call-template name="object.id"/>
    </xsl:variable>
    <xsl:variable name="master-reference">
      <xsl:call-template name="select.pagemaster"/>
    </xsl:variable>

    <fo:page-sequence id="{$id}"
                      hyphenate="{$hyphenate}"
                      master-reference="{$master-reference}">
      <xsl:attribute name="language">
        <xsl:call-template name="l10n.language"/>
      </xsl:attribute>

      <xsl:if test="not(preceding::chapter) and not(preceding::part)">
        <xsl:attribute name="initial-page-number">1</xsl:attribute>
      </xsl:if>

      <xsl:if test="$double.sided != 0">
        <xsl:attribute name="force-page-count">end-on-even</xsl:attribute>
      </xsl:if>

      <xsl:apply-templates select="." mode="running.head.mode">
        <xsl:with-param name="master-reference" select="$master-reference"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="running.foot.mode">
        <xsl:with-param name="master-reference" select="$master-reference"/>
      </xsl:apply-templates>

      <fo:flow flow-name="xsl-region-body">
        <fo:block id="{$id}">
          <xsl:call-template name="part.titlepage"/>
        </fo:block>
    
        <xsl:variable name="toc.params">
          <xsl:call-template name="find.path.params">
            <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:if test="contains($toc.params, 'toc')">
          <xsl:call-template name="division.toc"/>
        </xsl:if>
      </fo:flow>
    </fo:page-sequence>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>


<!-- Correct page numbers for CHAPTERS in main-TOC -->
<xsl:template match="chapter">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>
  <xsl:variable name="master-reference">
    <xsl:call-template name="select.pagemaster"/>
  </xsl:variable>

  <fo:page-sequence id="{$id}"
                    hyphenate="{$hyphenate}"
                    master-reference="{$master-reference}">
    <xsl:attribute name="language">
      <xsl:call-template name="l10n.language"/>
    </xsl:attribute>
    <xsl:attribute name="format">
      <xsl:call-template name="page.number.format"/>
    </xsl:attribute>
    <xsl:choose>
      <xsl:when test="not(preceding::chapter
                          or preceding::appendix
                          or preceding::article
                          or preceding::dedication
                          or parent::part
                          or parent::reference)">
        <xsl:attribute name="initial-page-number">1</xsl:attribute>
      </xsl:when>
      <xsl:when test="$double.sided != 0">
        <xsl:attribute name="initial-page-number">auto-odd</xsl:attribute>
      </xsl:when>
    </xsl:choose>

    <xsl:apply-templates select="." mode="running.head.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="running.foot.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <fo:flow flow-name="xsl-region-body">
      <fo:block id="{$id}">
        <xsl:call-template name="chapter.titlepage"/>
      </fo:block>

      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:call-template name="component.toc"/>
        <xsl:call-template name="component.toc.separator"/>
      </xsl:if>
      <xsl:apply-templates/>
    </fo:flow>
  </fo:page-sequence>
</xsl:template>

<!-- Correct page numbers for APPENDICES in main-TOC,
     and make appendix 'Resources' landscape pages   -->
<xsl:template match="appendix">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="master-reference">
    <xsl:choose>
      <xsl:when test="@id='resource'">
        <xsl:value-of select="'landscape-first'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="select.pagemaster"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <fo:page-sequence id="{$id}"
                    hyphenate="{$hyphenate}"
                    master-reference="{$master-reference}">
    <xsl:attribute name="language">
      <xsl:call-template name="l10n.language"/>
    </xsl:attribute>
    <xsl:attribute name="format">
      <xsl:call-template name="page.number.format"/>
    </xsl:attribute>
    <xsl:choose>
      <xsl:when test="not(preceding::chapter
                          or preceding::appendix
                          or preceding::article
                          or preceding::dedication
                          or parent::part
                          or parent::reference)">
        <!-- if there is a preceding component or we're in a part, the -->
        <!-- page numbering will already be adjusted -->
        <xsl:attribute name="initial-page-number">1</xsl:attribute>
      </xsl:when>
      <xsl:when test="$double.sided != 0">
        <xsl:attribute name="initial-page-number">auto-odd</xsl:attribute>
      </xsl:when>
    </xsl:choose>

    <xsl:apply-templates select="." mode="running.head.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <xsl:apply-templates select="." mode="running.foot.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <fo:flow flow-name="xsl-region-body">
      <fo:block id="{$id}">
        <xsl:call-template name="appendix.titlepage"/>
      </fo:block>

      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:call-template name="component.toc"/>
        <xsl:call-template name="component.toc.separator"/>
      </xsl:if>
      <xsl:apply-templates/>
    </fo:flow>
  </fo:page-sequence>
</xsl:template>


<!-- Correct page numbers for the PREFACE in main-TOC -->
<xsl:template match="preface">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="master-reference">
    <xsl:call-template name="select.pagemaster"/>
  </xsl:variable>

  <fo:page-sequence id="{$id}"
                    hyphenate="{$hyphenate}"
                    master-reference="{$master-reference}">
    <xsl:attribute name="language">
      <xsl:call-template name="l10n.language"/>
    </xsl:attribute>
    <xsl:attribute name="format">
      <xsl:call-template name="page.number.format"/>
    </xsl:attribute>

    <xsl:if test="$double.sided != 0">
      <xsl:attribute name="initial-page-number">auto-odd</xsl:attribute>
    </xsl:if>

    <xsl:apply-templates select="." mode="running.head.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <xsl:apply-templates select="." mode="running.foot.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <fo:flow flow-name="xsl-region-body">
      <fo:block id="{$id}">
        <xsl:call-template name="preface.titlepage"/>
      </fo:block>

      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:call-template name="component.toc"/>
        <xsl:call-template name="component.toc.separator"/>
      </xsl:if>

      <xsl:apply-templates/>
    </fo:flow>
  </fo:page-sequence>
</xsl:template>


<!-- Correct page numbers for the REFERENCE in main-TOC -->
<xsl:template match="reference">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>
  <xsl:variable name="master-reference">
    <xsl:call-template name="select.pagemaster"/>
  </xsl:variable>

  <fo:page-sequence id="{$id}"
                    hyphenate="{$hyphenate}"
                    master-reference="{$master-reference}">
    <xsl:attribute name="language">
      <xsl:call-template name="l10n.language"/>
    </xsl:attribute>
    <xsl:attribute name="format">
      <xsl:call-template name="page.number.format"/>
    </xsl:attribute>
    <xsl:if test="$double.sided != 0">
      <xsl:attribute name="initial-page-number">auto-odd</xsl:attribute>
    </xsl:if>

    <xsl:apply-templates select="." mode="running.head.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="running.foot.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <fo:flow flow-name="xsl-region-body">
      <fo:block id="{$id}">
        <xsl:call-template name="reference.titlepage"/>
      </fo:block>

      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:call-template name="component.toc"/>
        <xsl:call-template name="component.toc.separator"/>
      </xsl:if>
    </fo:flow>
  </fo:page-sequence>
  <xsl:apply-templates/>
</xsl:template>

<!-- ====================  END PAGENUMBERS IN TOC  ======================= -->


<!-- Show also the level below <PART> (chapter) in the MAIN-TOC -->
<xsl:template match="part" mode="toc">
  <xsl:param name="toc-context" select="."/>

  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="cid">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="$toc-context"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:call-template name="toc.line"/>

  <xsl:variable name="nodes" select="chapter|appendix|preface|reference"/>

  <fo:block id="toc.{$cid}.{$id}"
            start-indent="{count(ancestor::*)*$toc.indent.width}pt">
    <xsl:apply-templates select="$nodes" mode="toc">
      <xsl:with-param name="toc-context" select="$toc-context"/>
    </xsl:apply-templates>
  </fo:block>
</xsl:template>


<!-- Create also an TOC for the FAQ-chapter (1)-->
<xsl:template name="component.toc">
  <xsl:param name="toc-context" select="."/>

  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="cid">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="$toc-context"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="nodes" select="section|sect1|refentry
                                     |article|bibliography|glossary
                                     |appendix
                                     |qandaset"/>
  <xsl:if test="$nodes">
    <fo:block id="toc...{$id}"
              xsl:use-attribute-sets="toc.margin.properties">
      <xsl:call-template name="table.of.contents.titlepage"/>
      <xsl:apply-templates select="$nodes" mode="toc">
        <xsl:with-param name="toc-context" select="$toc-context"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:if>
</xsl:template>


<!-- Create also an TOC for the FAQ-chapter (2) -->
<xsl:template match="qandaset" mode="toc">
  <xsl:param name="toc-context" select="."/>

  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="cid">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="$toc-context"/>
    </xsl:call-template>
  </xsl:variable>

  <fo:block id="toc.{$cid}.{$id}">
    <xsl:apply-templates select="qandaentry" mode="toc"/>
  </fo:block>
</xsl:template>

<!-- Create also an TOC for the FAQ-chapter (3) -->
<xsl:template match="qandaentry" mode="toc">
  <xsl:param name="toc-context" select="."/>

  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="label">
    <xsl:apply-templates select="question" mode="label.markup"/>
  </xsl:variable>

  <fo:block text-align-last="justify"
            end-indent="{$toc.indent.width}pt"
            last-line-end-indent="-{$toc.indent.width}pt">
    <fo:inline keep-with-next.within-line="always">
      <fo:basic-link internal-destination="{$id}">
        <xsl:if test="$label != ''">
          <xsl:copy-of select="$label"/>
          <xsl:value-of select="$autotoc.label.separator"/>
        </xsl:if>
        <xsl:value-of select="question/*[text()]"/>
      </fo:basic-link>
    </fo:inline>
    <fo:inline keep-together.within-line="always">
      <xsl:text> </xsl:text>
      <fo:leader leader-pattern="dots"
                 leader-pattern-width="3pt"
                 leader-alignment="reference-area"
                 keep-with-next.within-line="always"/>
      <xsl:text> </xsl:text> 
      <fo:basic-link internal-destination="{$id}">
        <fo:page-number-citation ref-id="{$id}"/>
      </fo:basic-link>
    </fo:inline>
  </fo:block>
</xsl:template>

<!-- =======================    END TOC   ================================= -->



<!-- =======================   TITLEPAGE  ================================= -->

<!-- delete the empty page in the titlepage -->
<xsl:template name="book.titlepage">
  <xsl:call-template name="book.titlepage.recto"/>
  <xsl:call-template name="book.titlepage.before.verso"/>
  <xsl:call-template name="book.titlepage.verso"/>
</xsl:template>


<!-- Correct the NAMELISTS (authors, editors, translators) -->
<xsl:template name="book.titlepage.recto">
  <xsl:apply-templates select="title" mode="book.titlepage.recto.auto.mode"/>
  <xsl:apply-templates select="bookinfo" mode="php.contrib.lists"/>
</xsl:template>

<xsl:template match="bookinfo/authorgroup" mode="php.contrib.lists">
  <xsl:choose>
    <xsl:when test="@id='authors'">
      <fo:block  font-weight="bold" text-align="center" font-size="16pt" space-before="1.5in">
        <xsl:call-template name="gentext">
          <xsl:with-param name="key" select="'by'"/>
        </xsl:call-template>
      </fo:block>
      <fo:block text-align="center" font-size="12pt" space-before="1em">
        <xsl:call-template name="person.name.list"/>
      </fo:block>
    </xsl:when>
    <xsl:when test="@id='editors'">
      <fo:block  font-weight="bold" text-align="center" font-size="16pt" space-before="1.5in">
        <xsl:call-template name="gentext">
          <xsl:with-param name="key" select="'editedby'"/>
        </xsl:call-template>
      </fo:block>
      <fo:block text-align="center" font-size="12pt" space-before="1em">
        <xsl:call-template name="person.name.list"/>
      </fo:block>
    </xsl:when>
    <xsl:when test="@id='translators'">
      <fo:block text-align="center" font-weight="bold" font-size="12pt" space-before="1.5in">
        <xsl:apply-templates select="collab[1]/collabname"/>
      </fo:block>
      <fo:block text-align="center" font-size="12pt" space-before="1em">
        <xsl:apply-templates select="collab" mode="titlepage.mode"/>
      </fo:block>
    </xsl:when>
    <xsl:otherwise>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="collab" mode="titlepage.mode">
  <xsl:choose>
    <xsl:when test="position()=last()">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:when test="position() &gt; 1">
      <xsl:apply-templates/><xsl:text>, </xsl:text>
    </xsl:when>
    <xsl:otherwise>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- Prevent the AUTHOR-LISTS from appearing also on the 2ND TITLEPAGE -->
<xsl:template name="book.titlepage.verso">
  <xsl:apply-templates mode="book.titlepage.verso.auto.mode" select="title"/>
  <xsl:apply-templates mode="book.titlepage.verso.auto.mode" select="bookinfo/pubdate"/>
  <xsl:apply-templates mode="book.titlepage.verso.auto.mode" select="bookinfo/copyright"/>
  <xsl:apply-templates mode="book.titlepage.verso.auto.mode" select="bookinfo/legalnotice"/>
</xsl:template>

<xsl:template name="book.verso.title">
  <fo:block space-after="6.5in">
    <xsl:value-of select="/book/title"/>
  </fo:block>
</xsl:template>

<!-- ==========================  END TITLEPAGE  =========================== -->


<!-- ==================  REFERENCE/PARTINTRO TITLEPAGE  =================== -->

<!-- Suppress REFERENCE-TITLE on the the 2nd page -->
<xsl:template match="reference/partintro">
  <xsl:variable name="id">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="ancestor::reference"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="master-reference">
    <xsl:call-template name="select.pagemaster"/>
  </xsl:variable>

  <fo:page-sequence id="{$id}"
                    hyphenate="{$hyphenate}"
                    master-reference="{$master-reference}">
    <xsl:attribute name="language">
      <xsl:call-template name="l10n.language"/>
    </xsl:attribute>
    <xsl:if test="$double.sided != 0">
      <xsl:attribute name="force-page-count">end-on-even</xsl:attribute>
    </xsl:if>

    <xsl:apply-templates select="." mode="running.head.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="running.foot.mode">
      <xsl:with-param name="master-reference" select="$master-reference"/>
    </xsl:apply-templates>

    <fo:flow flow-name="xsl-region-body">
      <xsl:call-template name="partintro.titlepage"/>
      <xsl:variable name="toc.params">
        <xsl:call-template name="find.path.params">
          <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:if test="contains($toc.params, 'toc')">
        <xsl:apply-templates select="reference" mode="toc"/>
      </xsl:if>
      <xsl:apply-templates/>
    </fo:flow>
  </fo:page-sequence>
</xsl:template>

<!-- ==================== END REFERENCE/PARTINTRO TITLEPAGE =================== -->


<!-- Just to save space -->
<xsl:template match="refsect1">
  <fo:block>
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="refsect1/title">
  <fo:block font-family="{$title.font.family}" space-before="1em" space-after="0.9em" font-size="18pt" font-weight="bold">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="refnamediv/refpurpose">
  <xsl:apply-templates/>
</xsl:template>



<!-- Let NOTES look more similar to the html-dsssl-version -->
<xsl:template match="simpara|para|title" mode="note.single.entry">
  <xsl:apply-templates/>
</xsl:template>

<!-- FIXME: It's just a simple assumption that the title is
     always first, and the next node is always a (sim)para -->
<xsl:template match="note">
  <xsl:variable name="inlinepara">
    <xsl:choose>
      <xsl:when test="count(title) = 0">
        <xsl:apply-templates select="*[1]" mode="note.single.entry" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*[2]" mode="note.single.entry" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <fo:block space-before.minimum="0.8em"
            space-before.optimum="1em"
            space-before.maximum="1.2em"
            start-indent="0.25in"
            end-indent="0.25in">
    <fo:block keep-with-next="always">
      <fo:inline font-weight="bold">
        <xsl:choose>
          <xsl:when test="count(title) > 0">
            <xsl:apply-templates select="title" mode="note.single.entry"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="gentext"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>: </xsl:text>
      </fo:inline>
      <xsl:copy-of select="$inlinepara"/>
    </fo:block>
      <xsl:choose>
        <xsl:when test="$inlinepara != '' and count(title) > 0">
          <xsl:apply-templates select="*[not(position() &lt; 3)]" />
        </xsl:when>
        <xsl:when test="$inlinepara != '' and count(title) = 0">
          <xsl:apply-templates select="*[not(position() &lt; 2)]" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates />
        </xsl:otherwise>
      </xsl:choose>
  </fo:block>
</xsl:template>


<!-- Make the PROTOTYPE an own paragraph -->
<xsl:template match="methodsynopsis">
  <fo:block font-size="11pt">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<!-- Make the METHODNAMES bold  -->
<xsl:template match="methodsynopsis/methodname">
  <xsl:call-template name="inline.boldseq"/>
</xsl:template>


<!-- Make parameters just italic,
     ital.-monoseq is hard to read -->
<xsl:template match="parameter">
  <xsl:call-template name="inline.italicseq"/>
</xsl:template>

<!-- Make Methodparam's DEFAULT VALUES italic -->
<xsl:template match="methodparam/initializer">
  <xsl:text>=</xsl:text>
  <xsl:call-template name="inline.italicseq"/>
</xsl:template>



<!-- Make FUNCTIONS bold, add parenthesis, and if target
     exists and it's no title, link them to their refentry  -->
<xsl:template match="function">
  <xsl:variable name="content">
    <xsl:apply-templates/>
    <xsl:text>()</xsl:text>
  </xsl:variable>
  <xsl:variable name="targetid">
    <xsl:value-of select="concat('function.', translate(string(current()),'_','-'))"/>
  </xsl:variable>

  <xsl:call-template name="inline.boldseq">
    <xsl:with-param name="content">
      <xsl:choose>
        <xsl:when test="ancestor::refentry/refnamediv/refname=translate(current(),
                          'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')
                        or count(/*/part[@id='funcref']/*/refentry[@id=$targetid]) = 0">
          <xsl:copy-of select="$content"/>
        </xsl:when>
        <xsl:otherwise>
          <fo:basic-link internal-destination="{$targetid}">
            <xsl:copy-of select="$content"/>
          </fo:basic-link>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<!-- ===========================   F A Q  ================================= -->

<!-- Increase space for labels (for e.g. '10.10.') -->
<xsl:template match="qandaset">
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>

  <fo:block id="{$id}">
    <xsl:if test="title">
      <xsl:apply-templates select="title"/>
    </xsl:if>

    <xsl:apply-templates select="*[name(.) != 'title'
                                 and name(.) != 'qandadiv'
                                 and name(.) != 'qandaentry']"/>
    <xsl:apply-templates select="qandadiv"/>

    <xsl:if test="qandaentry">
      <fo:list-block xsl:use-attribute-sets="list.block.spacing"
                     provisional-distance-between-starts="2.8em"
                     provisional-label-separation="0.2em">
        <xsl:apply-templates select="qandaentry"/>
      </fo:list-block>
    </xsl:if>
  </fo:block>
</xsl:template>


<!-- Let FAQ-questions and answers look similar to the html-dsssl-version -->
<xsl:template match="question/para|question/simpara">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="question">
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>
  <xsl:variable name="entry.id">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="parent::*"/>
    </xsl:call-template>
  </xsl:variable>

  <fo:list-item id="{$entry.id}" xsl:use-attribute-sets="list.item.spacing">
    <fo:list-item-label end-indent="label-end()"> 
      <fo:block font-weight="bold">
        <xsl:apply-templates select="." mode="label.markup"/>
        <xsl:text>.</xsl:text>
      </fo:block>
    </fo:list-item-label>
    <fo:list-item-body start-indent="body-start()">
      <fo:block font-weight="bold">
        <xsl:apply-templates select="*[local-name(.)!='label']"/>
      </fo:block>
    </fo:list-item-body>
  </fo:list-item>
</xsl:template>

<xsl:template match="answer">
  <fo:list-item>
    <fo:list-item-label end-indent="label-end()"> 
      <fo:block/>
    </fo:list-item-label>
    <fo:list-item-body start-indent="body-start()">
      <xsl:apply-templates/>
    </fo:list-item-body>
  </fo:list-item>
</xsl:template>

<!-- =========================  END  F A Q  =============================== -->


<!-- ========================= APPENDIX RESOURCES ========================= -->

<!-- Create page-master for landscape-pages,
     is needed by the template 'appendix' above -->
<xsl:template name="user.pagemasters">
    <fo:simple-page-master master-name="landscape-first"
                           page-width="{$page.height.portrait}"
                           page-height="{$page.width.portrait}"
                           margin-top="{$page.margin.inner}"
                           margin-bottom="0.3in"
                           margin-left="{$page.margin.bottom}"
                           margin-right="{$page.margin.top}">
      <fo:region-body margin-bottom="{$body.margin.bottom}"
                      margin-top="{$body.margin.top}"
                      column-count="{$column.count.body}">
      </fo:region-body>
      <fo:region-before region-name="xsl-region-before-first"
                        extent="{$region.before.extent}"
                        display-align="before"/>
      <fo:region-after region-name="xsl-region-after-first"
                       extent="{$region.after.extent}"
                       display-align="after"/>
    </fo:simple-page-master>
</xsl:template>

<!-- Don't hyphenate function-names in Appendix 'Resources' -->
<xsl:template match="row[ancestor::appendix[@id='resource']]">
  <xsl:param name="spans"/>

  <fo:table-row hyphenate="false">
    <xsl:call-template name="anchor"/>

    <xsl:apply-templates select="(entry|entrytbl)[1]">
      <xsl:with-param name="spans" select="$spans"/>
    </xsl:apply-templates>
  </fo:table-row>

  <xsl:if test="following-sibling::row">
    <xsl:variable name="nextspans">
      <xsl:apply-templates select="(entry|entrytbl)[1]" mode="span">
        <xsl:with-param name="spans" select="$spans"/>
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:apply-templates select="following-sibling::row[1]">
      <xsl:with-param name="spans" select="$nextspans"/>
    </xsl:apply-templates>
  </xsl:if>
</xsl:template>

<!-- ======================= END APPENDIX RESOURCES ======================= -->



<!-- Some EXAMPLES need MORE SPACE, unify the
     inner width of the several page-formats -->
<xsl:param name="page.margin.inner">
  <xsl:choose>
    <xsl:when test="$paper.type = 'A4'">18mm</xsl:when>
    <xsl:otherwise>20mm</xsl:otherwise>
  </xsl:choose>
</xsl:param>
<xsl:param name="page.margin.outer">
  <xsl:choose>
    <xsl:when test="$paper.type = 'A4'">16mm</xsl:when>
    <xsl:otherwise>19.9mm</xsl:otherwise>
  </xsl:choose>
</xsl:param>


<xsl:attribute-set name="section.title.level1.properties">
  <xsl:attribute name="font-size">20pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="section.title.level2.properties">
  <xsl:attribute name="font-size">18pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="section.title.level3.properties">
  <xsl:attribute name="font-size">14pt</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="section.title.level4.properties">
  <xsl:attribute name="font-size">12pt</xsl:attribute>
</xsl:attribute-set>

<!-- Adjust size and spaces for titles of examples, figures, etc. -->
<xsl:attribute-set name="formal.title.properties">
  <xsl:attribute name="font-weight">bold</xsl:attribute>
  <xsl:attribute name="font-size">11pt</xsl:attribute>
  <xsl:attribute name="hyphenate">false</xsl:attribute>
  <xsl:attribute name="space-before">2pt</xsl:attribute>
  <xsl:attribute name="space-after">3pt</xsl:attribute>
</xsl:attribute-set>


<!-- Show the titles of INDEXDIV and omit error-message -->
<xsl:template name="indexdiv.title">
  <xsl:param name="title"/>
  <xsl:param name="titlecontent"/>
  <fo:block margin-left="{$title.margin.left}"
	    font-size="16pt"
            font-family="{$title.font.family}"
            font-weight="bold"
            keep-with-next.within-column="always"
            space-before="10pt"
            space-after="10pt">
    <xsl:choose>
      <xsl:when test="$title">
        <xsl:value-of select="$title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$titlecontent"/>
      </xsl:otherwise>
    </xsl:choose>
  </fo:block>
</xsl:template>


<xsl:attribute-set name="block.spacing.small">
  <xsl:attribute name="space-before.optimum">0.4em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.2em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.6em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">0.4em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.2em</xsl:attribute>
  <xsl:attribute name="space-after.maximum">0.6em</xsl:attribute>
</xsl:attribute-set>

<!-- add width to fo:table to prevent fop-error, and
     make smaller spacing before and after the list  -->
<xsl:template match="simplelist">
  <fo:table xsl:use-attribute-sets="block.spacing.small" width="100%">
    <xsl:call-template name="simplelist.table.columns">
      <xsl:with-param name="cols">
        <xsl:choose>
          <xsl:when test="@columns">
            <xsl:value-of select="@columns"/>
          </xsl:when>
          <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
    <fo:table-body>
      <xsl:call-template name="simplelist.vert">
	<xsl:with-param name="cols">
	  <xsl:choose>
	    <xsl:when test="@columns">
	      <xsl:value-of select="@columns"/>
	    </xsl:when>
	    <xsl:otherwise>1</xsl:otherwise>
	  </xsl:choose>
	</xsl:with-param>
      </xsl:call-template>
    </fo:table-body>
  </fo:table>
</xsl:template>


<!-- Enable hyphenating URL's (for fop!) -->
<xsl:param name="ulink.hyphenate" select="'&#x200B;'"/>

<!-- Don't show the URL, if linked text=URL;  if
     URL is shown, include it into the basic-link -->
<xsl:template match="ulink" name="ulink">
  <fo:basic-link xsl:use-attribute-sets="xref.properties">
    <xsl:attribute name="external-destination">
      <xsl:call-template name="fo-external-image">
        <xsl:with-param name="filename" select="@url"/>
      </xsl:call-template>
    </xsl:attribute>

    <xsl:choose>
      <xsl:when test="count(child::node())=0 or
                      normalize-space(string(.)) = @url">
        <xsl:call-template name="hyphenate-url">
          <xsl:with-param name="url" select="@url"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	    <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:if test="count(child::node()) != 0
                  and normalize-space(string(.)) != @url
                  and $ulink.show != 0">
      <xsl:text> [</xsl:text>
      <xsl:call-template name="hyphenate-url">
        <xsl:with-param name="url" select="@url"/>
      </xsl:call-template>
      <xsl:text>]</xsl:text>
    </xsl:if>
  </fo:basic-link>
</xsl:template>



<!-- Align titles of chapters, appendices, etc. left
     (to prevent the ugly spacing on long titles)   -->
<xsl:template name="component.title">
  <xsl:param name="node" select="."/>
  <xsl:param name="pagewide" select="0"/>
  <xsl:variable name="id">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="$node"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="title">
    <xsl:apply-templates select="$node" mode="object.title.markup">
      <xsl:with-param name="allow-anchors" select="1"/>
    </xsl:apply-templates>
  </xsl:variable>
  <xsl:variable name="titleabbrev">
    <xsl:apply-templates select="$node" mode="titleabbrev.markup"/>
  </xsl:variable>

  <xsl:if test="$passivetex.extensions != 0">
    <fotex:bookmark xmlns:fotex="http://www.tug.org/fotex"
                    fotex-bookmark-level="2"
                    fotex-bookmark-label="{$id}">
      <xsl:value-of select="$titleabbrev"/>
    </fotex:bookmark>
  </xsl:if>

  <fo:block keep-with-next.within-column="always"
            space-before.optimum="{$body.font.master}pt"
            space-before.minimum="{$body.font.master * 0.8}pt"
            space-before.maximum="{$body.font.master * 1.2}pt"
            text-align="left"
            hyphenate="false">
    <xsl:if test="$pagewide != 0">
      <xsl:attribute name="span">all</xsl:attribute>
    </xsl:if>
    <xsl:copy-of select="$title"/>
  </fo:block>
</xsl:template>



</xsl:stylesheet>
