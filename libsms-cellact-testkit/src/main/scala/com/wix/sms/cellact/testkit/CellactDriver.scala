package com.wix.sms.cellact.testkit

import java.util.concurrent.atomic.AtomicReference
import java.util.{List => JList}

import akka.http.scaladsl.model._
import com.google.api.client.http.UrlEncodedParser
import com.wix.e2e.http.RequestHandler
import com.wix.sms.cellact.model._
import com.wix.sms.cellact.{CellactHelper, Credentials}

import com.wix.e2e.http.client.extractors.HttpMessageExtractors._

import scala.collection.JavaConversions._
import scala.collection.mutable
import com.wix.e2e.http.server.WebServerFactory.aMockWebServerWith

class CellactDriver(port: Int) {
  private val delegatingHandler: RequestHandler = { case r: HttpRequest => handler.get().apply(r) }
  private val notFoundHandler: RequestHandler = { case _: HttpRequest => HttpResponse(status = StatusCodes.NotFound) }

  private val handler = new AtomicReference(notFoundHandler)

  private val probe = aMockWebServerWith(delegatingHandler).onPort(port).build
  private val paloParser = new PaloParser
  private val responseParser = new ResponseParser

  def start() {
    probe.start()
  }

  def stop() {
    probe.stop()
  }

  def reset() {
    handler.set(notFoundHandler)
  }

  def aSendPlainFor(credentials: Credentials, source: String, destPhone: String, text: String): SendCtx = {
    new SendCtx(
      credentials = credentials,
      source = source,
      destPhone = destPhone,
      text = text)
  }

  def aSendUnicodeFor(credentials: Credentials, source: String, destPhone: String, text: String): SendCtx = {
    new SendCtx(
      credentials = credentials,
      source = source,
      destPhone = destPhone,
      text = text)
  }

  class SendCtx(credentials: Credentials, source: String, destPhone: String, text: String) {
    private val expectedPalo = CellactHelper.createPalo(
      credentials = credentials,
      sender = source,
      destPhone = destPhone,
      text = text
    )

    def returns(msgId: String): Unit = {
      val response = new Response
      response.RESULTCODE = ResultCodes.success
      response.RESULTMESSAGE = "Success"
      response.BLMJ = msgId

      val responseXml = responseParser.stringify(response)
      returnsXml(responseXml)
    }

    def failsWith(code: String, message: String): Unit = {
      val response = new Response
      response.RESULTCODE = code
      response.RESULTMESSAGE = message

      val responseXml = responseParser.stringify(response)
      returnsXml(responseXml)
    }

    private def prependHandler(handle: RequestHandler) =
      handler.set(handle orElse handler.get())

    private def returnsXml(responseXml: String): Unit = {
      prependHandler({
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/"),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentType(MediaTypes.`text/xml`, HttpCharsets.`UTF-8`), responseXml))
      })
    }

    private def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      val requestParams = urlDecode(entity.extractAsString)

      val requestXml = requestParams(Fields.xmlString)
      val palo = paloParser.parse(requestXml)

      palo == expectedPalo
    }

    private def urlDecode(str: String): Map[String, String] = {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(str, mutableMapAsJavaMap(params))
      params.mapValues( _.head ).toMap
    }
  }
}
