#' Tool: Provide dictionary of HKO' weather camera station code
#'
#' @return
#' @export
#'
#' @examples tool_hk_wxph()
tool_hk_wxph = function(){
  wxph = tibble::tibble(LFS = "Lau Fau Shan (W)",
                        WLP = "Wetland Park (NE)",
                        ELC = "Elegantia College in Sheung Shui (NW)",
                        KFB = "Kadoorie Farm and Botanic Garden (W)",
                        TPK = "Tai Po Kau (NE)",
                        TM2 = "Tai Mo Shan (SW)",
                        TM3 = "Tai Mo Shan (NE)",
                        TLC = "Tai Lam Chung (S)",
                        SK2 = "Sai Kung Marine East Station (NE)",
                        SKG = "Sai Kung Marine East Station (SE)",
                        CWB = "Clear Water Bay (SW)",
                        CWA = "Clear Water Bay (E)",
                        KS2 = "Kau Sai Chau (WNW)",
                        KLT = "Kowloon City (SE)",
                        HK2 = "Tsim Sha Tsui (W)",
                        HKO = "Tsim Sha Tsui (E)",
                        IC2 = "International Commerce Centre (SW)",
                        IC1 = "International Commerce Centre (SE)",
                        CP1 = "Center (E)",
                        VPB = "Victoria Peak (NNE)",
                        VPA = "Victoria Peak (E)",
                        GSI = "German Swiss International School",
                        SWH = "Sai Wan Ho (E)",
                        SLW = "Sha Lo Wan (NE)",
                        DNL = "Peng Chau (N)",
                        PE2 = "Peng Chau (E)",
                        CCH = "Cheung Chau (N)",
                        CCE = "Cheung Chau Tung Wan (E)",
                        LAM = "Lamma Island (NW)",
                        WL2 = "Waglan Island (NNW)",
                        WGL = "Waglan Island (W)") %>%
    tidyr::pivot_longer(cols = tidyr::everything(), names_to = "code", values_to = "full_name")
  return(wxph)
}
